package io.github.edadma.parsyl

import scala.util.matching.Regex as ScalaRegex

// Position in a source string.
//   Sysl: struct Input { source: string, offset: int }
case class Input(source: String, offset: Int):
  def atEnd: Boolean = offset >= source.length
  def advance(n: Int): Input = Input(source, offset + n)
  def peek: Char = source.charAt(offset)
  def remaining: String = source.substring(offset)

// Outcome of running a parser. Both arms carry an Input — Success points at the
// position after the matched text, Failure points at where the match failed.
//   Sysl: enum ParseResult[T] { Success(value: T, next: Input); Failure(msg: string, at: Input) }
enum ParseResult[A]:
  case Success(value: A, next: Input)
  case Failure(msg: String, at: Input)

// A parser is a function from Input to ParseResult[A].
//   Sysl: type Parser[A] = new (Input) -> ParseResult[A]
//
// The `new` makes each instantiation (Parser[i32], Parser[string], …) a
// distinct nominal type, so trait/impl dispatch and operator overloading can
// bind on it. Direct call syntax `parser(in)` is preserved (no struct wrapper
// with a `.fn` field). Available on sysl dev as of bea5d7099.
type Parser[A] = Input => ParseResult[A]

// All combinators are top-level functions taking and returning Parser values.
// Nothing relies on Scala-only features (implicits, extension methods, HKTs,
// call-by-name) — every signature maps 1:1 to a Sysl generic function.
object Parsers:
  import ParseResult.*

  // ---- primitive parsers ----

  def literal(s: String): Parser[String] = (in: Input) =>
    if in.source.startsWith(s, in.offset) then
      Success(s, in.advance(s.length))
    else
      Failure(s"expected literal '$s'", in)

  def regex(pattern: String): Parser[String] =
    val r = pattern.r
    (in: Input) =>
      r.findPrefixOf(in.source.substring(in.offset)) match
        case Some(m) => Success(m, in.advance(m.length))
        case None    => Failure(s"expected match for /$pattern/", in)

  def acceptIf(label: String, pred: Char => Boolean): Parser[Char] = (in: Input) =>
    if in.atEnd then Failure(s"expected $label, got end of input", in)
    else if pred(in.peek) then Success(in.peek, in.advance(1))
    else Failure(s"expected $label, got '${in.peek}'", in)

  def end: Parser[Unit] = (in: Input) =>
    if in.atEnd then Success((), in)
    else Failure(s"expected end of input, got '${in.peek}'", in)

  def success[A](v: A): Parser[A] = (in: Input) => Success(v, in)
  def failure[A](msg: String): Parser[A] = (in: Input) => Failure(msg, in)

  // ---- transformers ----

  def map[A, B](p: Parser[A], f: A => B): Parser[B] = (in: Input) =>
    p(in) match
      case Success(v, n) => Success(f(v), n)
      case Failure(m, n) => Failure(m, n)

  def flatMap[A, B](p: Parser[A], f: A => Parser[B]): Parser[B] = (in: Input) =>
    p(in) match
      case Success(v, n) => f(v)(n)
      case Failure(m, n) => Failure(m, n)

  // ---- sequencing ----

  def seq[A, B](p: Parser[A], q: Parser[B]): Parser[(A, B)] = (in: Input) =>
    p(in) match
      case Failure(m, n) => Failure(m, n)
      case Success(va, na) =>
        q(na) match
          case Failure(m, n)   => Failure(m, n)
          case Success(vb, nb) => Success((va, vb), nb)

  def seqL[A, B](p: Parser[A], q: Parser[B]): Parser[A] = map(seq(p, q), t => t._1)
  def seqR[A, B](p: Parser[A], q: Parser[B]): Parser[B] = map(seq(p, q), t => t._2)

  // ---- alternation ----

  def or[A](p: Parser[A], q: Parser[A]): Parser[A] = (in: Input) =>
    p(in) match
      case s @ Success(_, _) => s
      case Failure(_, _)     => q(in)

  // ---- optional ----

  def opt[A](p: Parser[A]): Parser[Option[A]] = (in: Input) =>
    p(in) match
      case Success(v, n) => Success(Some(v), n)
      case Failure(_, _) => Success(None, in)

  // ---- repetition ----
  // The no-progress check (n.offset > current.offset) defends against a parser
  // that succeeds without consuming input — without it `rep(success(x))` would
  // loop forever. Same defence in rep1.

  def rep[A](p: Parser[A]): Parser[Vector[A]] = (in: Input) =>
    var result   = Vector.empty[A]
    var current  = in
    var continue = true
    while continue do
      p(current) match
        case Success(v, n) =>
          if n.offset > current.offset then
            result = result :+ v
            current = n
          else continue = false
        case Failure(_, _) => continue = false
    Success(result, current)

  def rep1[A](p: Parser[A]): Parser[Vector[A]] = (in: Input) =>
    p(in) match
      case Failure(m, n) => Failure(m, n)
      case Success(first, after) =>
        var result   = Vector(first)
        var current  = after
        var continue = true
        while continue do
          p(current) match
            case Success(v, n) =>
              if n.offset > current.offset then
                result = result :+ v
                current = n
              else continue = false
            case Failure(_, _) => continue = false
        Success(result, current)

  def repsep[A, S](p: Parser[A], sep: Parser[S]): Parser[Vector[A]] =
    or(rep1sep(p, sep), success(Vector.empty[A]))

  def rep1sep[A, S](p: Parser[A], sep: Parser[S]): Parser[Vector[A]] = (in: Input) =>
    p(in) match
      case Failure(m, n) => Failure(m, n)
      case Success(first, after) =>
        var result: Vector[A]            = Vector(first)
        var current                       = after
        var status: ParseResult[Vector[A]] = Success(result, current)
        var continue                       = true
        while continue do
          sep(current) match
            case Failure(_, _) =>
              status = Success(result, current)
              continue = false
            case Success(_, n1) =>
              p(n1) match
                case Failure(m, n) =>
                  status = Failure(m, n)
                  continue = false
                case Success(v, n2) =>
                  result = result :+ v
                  current = n2
        status

  // ---- left/right-associative chains ----
  // Capture the "head op tail op tail …" shape that recursive-descent grammars
  // hit at every precedence level. Without these, every non-terminal carries
  // the same boilerplate match: peel off the head, fold a `rep(op ~ operand)`
  // tail by hand. With chainl1 the whole non-terminal is one expression.
  //
  // The `op` parser produces a binary combiner — typically `op_token ^^^ fn`
  // where `fn: (A, A) => A`. Sysl: `chainl1[A](operand: Parser[A], op: Parser[(A, A) -> A]) -> Parser[A]`.

  /** Left-associative chain: `operand (op operand)*`, folded into the accumulator
   *  left-to-right via the function returned by each `op` match.
   *  When `op` succeeds but the next `operand` fails, the partial-op match is
   *  silently absorbed (cursor stays at the position before that op). */
  def chainl1[A](operand: Parser[A], op: Parser[(A, A) => A]): Parser[A] = (in: Input) =>
    operand(in) match
      case Failure(m, n) => Failure(m, n)
      case Success(first, after) =>
        var acc      = first
        var current  = after
        var continue = true
        while continue do
          op(current) match
            case Failure(_, _) => continue = false
            case Success(f, n1) =>
              operand(n1) match
                case Failure(_, _) => continue = false
                case Success(v, n2) =>
                  acc = f(acc, v)
                  current = n2
        Success(acc, current)

  /** Right-associative chain: `operand (op operand)*`, folded right-to-left.
   *  `a op b op c` becomes `f(a, f(b, c))`. */
  def chainr1[A](operand: Parser[A], op: Parser[(A, A) => A]): Parser[A] = (in: Input) =>
    operand(in) match
      case Failure(m, n) => Failure(m, n)
      case Success(first, after) =>
        var values: Vector[A]              = Vector(first)
        var ops:    Vector[(A, A) => A]    = Vector.empty
        var current                          = after
        var continue                         = true
        while continue do
          op(current) match
            case Failure(_, _) => continue = false
            case Success(f, n1) =>
              operand(n1) match
                case Failure(_, _) => continue = false
                case Success(v, n2) =>
                  ops = ops :+ f
                  values = values :+ v
                  current = n2
        var acc = values(values.length - 1)
        var i   = values.length - 2
        while i >= 0 do
          acc = ops(i)(values(i), acc)
          i -= 1
        Success(acc, current)

  // ---- recursion helper ----
  // For non-left-recursive grammars the standard idiom is:
  //   def expr: Parser[T] = (in: Input) => ...body referencing expr...(in)
  // Wrapping the body in a lambda defers evaluation of recursive references
  // until the parser is actually applied to input. `lazyP` does the same thing
  // for a single sub-expression when wrapping the whole body would be awkward.
  def lazyP[A](thunk: () => Parser[A]): Parser[A] = (in: Input) => thunk()(in)

  // ---- whitespace ----

  def skipWS[A](p: Parser[A]): Parser[A] = (in: Input) =>
    var i = in.offset
    while i < in.source.length && in.source.charAt(i).isWhitespace do i += 1
    p(Input(in.source, i))

  // Run p, then skip trailing whitespace. Parsers built with `lex` compose
  // naturally because each one leaves the cursor on the next non-ws character.
  def lex[A](p: Parser[A]): Parser[A] = (in: Input) =>
    skipWS(p)(in) match
      case Failure(m, n) => Failure(m, n)
      case Success(v, after) =>
        var j = after.offset
        while j < after.source.length && after.source.charAt(j).isWhitespace do j += 1
        Success(v, Input(after.source, j))

  // ---- top-level entry ----

  def parseAll[A](p: Parser[A], source: String): ParseResult[A] =
    skipWS(seqL(p, skipWS(end)))(Input(source, 0))

// ----------------------------------------------------------------------------
// Operator sugar.
//
// These extension methods make grammars read like the Scala parser-combinator
// idiom:   number ~ "+" ~ number   /   ident | keyword   /   p ^^ (_.toInt)
//
// All six operators translate directly to Sysl on dev (≥ bea5d7099) via
// user-defined operator symbols (#operator) plus generic multi-parameter
// trait impls. Each extension method below shows its sysl shape — they all
// follow the same pattern:
//
//   trait Concat[A, B, R]
//       #operator("~")
//       concat(a: A, b: B) -> R
//
//   impl[A, B] Concat[Parser[A], Parser[B], Parser[(A, B)]]
//       concat(a: Parser[A], b: Parser[B]) -> Parser[(A, B)] = seq(a, b)
//
// The unifier (Stage F.3-G, da96cfd87) handles dispatch; nominal generic
// aliases (bea5d7099) make Parser[A] a struct-like nominal type that the
// LHS-must-be-struct-or-enum operator-binding rule accepts.
// ----------------------------------------------------------------------------

extension [A](p: Parser[A])
  // Sequence: run p then q, return the pair.
  //   trait Concat[A, B, R]   #operator("~")   concat(a: A, b: B) -> R
  //   impl[A, B] Concat[Parser[A], Parser[B], Parser[(A, B)]] = ... seq(a, b)
  def ~[B](q: Parser[B]): Parser[(A, B)] = Parsers.seq(p, q)

  // Alternation: try p, fall back to q.
  //   trait Or[T]   #operator("|")   or(a: T, b: T) -> T
  //   impl[A] Or[Parser[A]] = ... Parsers.or(a, b)
  def |(q: Parser[A]): Parser[A] = Parsers.or(p, q)

  // Sequence, keep the right value.
  //   trait SeqR[A, B, R]   #operator("~>")   seqr(a: A, b: B) -> R
  //   impl[A, B] SeqR[Parser[A], Parser[B], Parser[B]] = ... seqR(a, b)
  def ~>[B](q: Parser[B]): Parser[B] = Parsers.seqR(p, q)

  // Sequence, keep the left value.
  //   trait SeqL[A, B, R]   #operator("<~")   seql(a: A, b: B) -> R
  //   impl[A, B] SeqL[Parser[A], Parser[B], Parser[A]] = ... seqL(a, b)
  def <~[B](q: Parser[B]): Parser[A] = Parsers.seqL(p, q)

  // Map the success value through f.
  //   trait Map[A, F, R]   #operator("^^")   pmap(a: A, f: F) -> R
  //   impl[A, B] Map[Parser[A], (A) -> B, Parser[B]] = ... Parsers.map(a, f)
  def ^^[B](f: A => B): Parser[B] = Parsers.map(p, f)

  // Replace the success value with a constant.
  //   trait MapTo[A, V, R]   #operator("^^^")   pmapto(a: A, v: V) -> R
  //   impl[A, B] MapTo[Parser[A], B, Parser[B]] = ... Parsers.map(a, _ => v)
  def ^^^[B](v: B): Parser[B] = Parsers.map(p, _ => v)
