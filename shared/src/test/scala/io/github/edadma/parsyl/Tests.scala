package io.github.edadma.parsyl

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import io.github.edadma.parsyl.Parsers
import io.github.edadma.parsyl.Parsers.{regex as _, *}
import io.github.edadma.parsyl.ParseResult.*

class Tests extends AnyFreeSpec with Matchers:

  // ---- helpers ----

  private def successAt[A](r: ParseResult[A], expected: A, offset: Int): Unit =
    r match
      case Success(v, n) =>
        v shouldBe expected
        n.offset shouldBe offset
      case Failure(m, n) =>
        fail(s"expected success, got failure: $m at offset ${n.offset}")

  private def failsAt[A](r: ParseResult[A], offset: Int): Unit =
    r match
      case Success(v, n) => fail(s"expected failure, got success: $v at offset ${n.offset}")
      case Failure(_, n) => n.offset shouldBe offset

  // ---- Input ----

  "Input" - {
    "atEnd is false in the middle, true past the end" in {
      Input("abc", 0).atEnd shouldBe false
      Input("abc", 2).atEnd shouldBe false
      Input("abc", 3).atEnd shouldBe true
    }
    "advance moves the offset" in {
      Input("abc", 0).advance(2).offset shouldBe 2
    }
    "peek returns the current character" in {
      Input("abc", 1).peek shouldBe 'b'
    }
    "remaining returns the unconsumed suffix" in {
      Input("abcdef", 3).remaining shouldBe "def"
    }
  }

  // ---- literal ----

  "literal" - {
    "matches at the start" in {
      successAt(literal("hello")(Input("hello world", 0)), "hello", 5)
    }
    "matches mid-string when offset is set" in {
      successAt(literal("world")(Input("hello world", 6)), "world", 11)
    }
    "fails when prefix differs" in {
      failsAt(literal("foo")(Input("bar", 0)), 0)
    }
    "fails when input is shorter than literal" in {
      failsAt(literal("foobar")(Input("foo", 0)), 0)
    }
    "matches the empty literal trivially" in {
      successAt(literal("")(Input("anything", 0)), "", 0)
    }
  }

  // ---- regex ----

  "regex" - {
    "matches a digit run" in {
      successAt(Parsers.regex("[0-9]+")(Input("123abc", 0)), "123", 3)
    }
    "fails with no leading match" in {
      failsAt(Parsers.regex("[0-9]+")(Input("abc", 0)), 0)
    }
    "is anchored to the current offset" in {
      successAt(Parsers.regex("[a-z]+")(Input("123abc", 3)), "abc", 6)
      failsAt(Parsers.regex("[a-z]+")(Input("123abc", 0)), 0)
    }
  }

  // ---- acceptIf / end ----

  "acceptIf" - {
    "accepts a single character matching the predicate" in {
      successAt(acceptIf("digit", _.isDigit)(Input("7x", 0)), '7', 1)
    }
    "rejects a non-matching character" in {
      failsAt(acceptIf("digit", _.isDigit)(Input("x7", 0)), 0)
    }
    "rejects end of input" in {
      failsAt(acceptIf("digit", _.isDigit)(Input("", 0)), 0)
    }
  }

  "end" - {
    "succeeds at the end of input" in {
      successAt(end(Input("abc", 3)), (), 3)
    }
    "fails before the end" in {
      failsAt(end(Input("abc", 1)), 1)
    }
  }

  // ---- success / failure ----

  "success" - {
    "always succeeds without consuming" in {
      successAt(success(42)(Input("anything", 2)), 42, 2)
    }
  }

  "failure" - {
    "always fails without consuming" in {
      failsAt(failure[Int]("nope")(Input("anything", 2)), 2)
    }
  }

  // ---- map / flatMap ----

  "map" - {
    "transforms the success value" in {
      successAt(map(Parsers.regex("[0-9]+"), _.toInt)(Input("42", 0)), 42, 2)
    }
    "propagates failure unchanged" in {
      failsAt(map(literal("foo"), _.toUpperCase)(Input("bar", 0)), 0)
    }
  }

  "flatMap" - {
    "chains: parse a digit count, then that many letters" in {
      val p = flatMap(map(Parsers.regex("[0-9]"), _.toInt), n => Parsers.regex(s"[a-z]{$n}"))
      successAt(p(Input("3abc", 0)), "abc", 4)
    }
    "fails if the second parser fails" in {
      val p = flatMap(map(Parsers.regex("[0-9]"), _.toInt), n => Parsers.regex(s"[a-z]{$n}"))
      failsAt(p(Input("3ab", 0)), 1)
    }
  }

  // ---- seq / seqL / seqR ----

  "seq" - {
    "produces a tuple of the two results" in {
      seq(literal("foo"), literal("bar"))(Input("foobar", 0)) match
        case Success((a, b), n) =>
          a shouldBe "foo"
          b shouldBe "bar"
          n.offset shouldBe 6
        case Failure(m, _) => fail(m)
    }
    "fails if the first parser fails" in {
      failsAt(seq(literal("foo"), literal("bar"))(Input("xfoobar", 0)), 0)
    }
    "fails if the second parser fails" in {
      failsAt(seq(literal("foo"), literal("bar"))(Input("fooXXX", 0)), 3)
    }
  }

  "seqL" - {
    "keeps the left value, advances past the right" in {
      successAt(seqL(literal("foo"), literal("bar"))(Input("foobar", 0)), "foo", 6)
    }
  }

  "seqR" - {
    "keeps the right value, advances past the right" in {
      successAt(seqR(literal("foo"), literal("bar"))(Input("foobar", 0)), "bar", 6)
    }
  }

  // ---- or ----

  "or" - {
    "takes the first branch when it succeeds" in {
      successAt(or(literal("foo"), literal("bar"))(Input("foo", 0)), "foo", 3)
    }
    "falls back to the second branch when the first fails" in {
      successAt(or(literal("foo"), literal("bar"))(Input("bar", 0)), "bar", 3)
    }
    "fails when neither branch matches" in {
      failsAt(or(literal("foo"), literal("bar"))(Input("baz", 0)), 0)
    }
    "does NOT consume input on first-branch failure (no commit)" in {
      // first parser would consume 'fo' then fail on 'X' vs 'o'; second matches 'foX' literally
      successAt(or(literal("fooo"), literal("foX"))(Input("foX", 0)), "foX", 3)
    }
  }

  // ---- opt ----

  "opt" - {
    "returns Some on a successful match" in {
      successAt(opt(literal("foo"))(Input("foo", 0)), Some("foo"), 3)
    }
    "returns None without consuming on failure" in {
      successAt(opt(literal("foo"))(Input("bar", 0)), None, 0)
    }
  }

  // ---- rep / rep1 ----

  "rep" - {
    "matches zero or more occurrences" in {
      successAt(rep(literal("a"))(Input("aaab", 0)), Vector("a", "a", "a"), 3)
    }
    "succeeds with empty when there are no matches" in {
      successAt(rep(literal("a"))(Input("xxx", 0)), Vector.empty[String], 0)
    }
    "stops on a no-progress success (would otherwise infinite-loop)" in {
      successAt(rep(success(0))(Input("abc", 0)), Vector.empty[Int], 0)
    }
  }

  "rep1" - {
    "matches one or more occurrences" in {
      successAt(rep1(literal("a"))(Input("aaab", 0)), Vector("a", "a", "a"), 3)
    }
    "fails when there are zero matches" in {
      failsAt(rep1(literal("a"))(Input("xxx", 0)), 0)
    }
  }

  // ---- repsep / rep1sep ----

  "repsep" - {
    "parses comma-separated identifiers" in {
      val p = repsep(Parsers.regex("[a-z]+"), literal(","))
      successAt(p(Input("foo,bar,baz", 0)), Vector("foo", "bar", "baz"), 11)
    }
    "succeeds with empty when nothing matches" in {
      successAt(repsep(literal("a"), literal(","))(Input("xxx", 0)), Vector.empty[String], 0)
    }
    "matches a single element with no separator" in {
      successAt(repsep(literal("a"), literal(","))(Input("a", 0)), Vector("a"), 1)
    }
  }

  "rep1sep" - {
    "fails on empty input" in {
      failsAt(rep1sep(literal("a"), literal(","))(Input("", 0)), 0)
    }
    "fails when the separator is followed by a non-match (commits to sep)" in {
      // 'a' matches, then ',' matches, then we need 'a' but we get 'X' — must fail.
      failsAt(rep1sep(literal("a"), literal(","))(Input("a,X", 0)), 2)
    }
  }

  // ---- lazyP ----

  "lazyP" - {
    "defers thunk evaluation until the parser runs" in {
      var built = 0
      lazy val p: Parser[String] = lazyP { () =>
        built += 1
        literal("x")
      }
      built shouldBe 0
      successAt(p(Input("x", 0)), "x", 1)
      built shouldBe 1
      successAt(p(Input("x", 0)), "x", 1)
      built shouldBe 2
    }
  }

  // ---- whitespace ----

  "skipWS" - {
    "skips leading whitespace before the inner parser" in {
      successAt(skipWS(literal("foo"))(Input("   foo", 0)), "foo", 6)
    }
    "is a no-op when there is no leading whitespace" in {
      successAt(skipWS(literal("foo"))(Input("foo", 0)), "foo", 3)
    }
  }

  "lex" - {
    "skips both leading and trailing whitespace" in {
      successAt(lex(literal("foo"))(Input("  foo   bar", 0)), "foo", 8)
    }
    "tokens chain naturally" in {
      val p = seq(lex(literal("foo")), lex(literal("bar")))
      p(Input("  foo   bar  ", 0)) match
        case Success((a, b), n) =>
          a shouldBe "foo"
          b shouldBe "bar"
          n.offset shouldBe 13
        case Failure(m, _) => fail(m)
    }
  }

  // ---- parseAll ----

  "parseAll" - {
    "trims surrounding whitespace and requires full input" in {
      successAt(parseAll(literal("hi"), "  hi  "), "hi", 6)
    }
    "fails if there is unconsumed non-whitespace input" in {
      failsAt(parseAll(literal("hi"), "hi extra"), 3)
    }
  }

  // ---- operator sugar ----
  // These mirror the named-function tests but go through the extension
  // methods. They confirm that `~`, `|`, `~>`, `<~`, `^^`, `^^^` desugar
  // to seq/or/seqR/seqL/map exactly.

  "operator sugar" - {

    "~ sequences and produces a tuple" in {
      (literal("foo") ~ literal("bar"))(Input("foobar", 0)) match
        case Success((a, b), n) =>
          a shouldBe "foo"; b shouldBe "bar"; n.offset shouldBe 6
        case Failure(m, _) => fail(m)
    }
    "~ fails if the first parser fails" in {
      failsAt((literal("foo") ~ literal("bar"))(Input("xfoo", 0)), 0)
    }
    "~ fails if the second parser fails" in {
      failsAt((literal("foo") ~ literal("bar"))(Input("fooXXX", 0)), 3)
    }

    "| takes the first matching branch" in {
      successAt((literal("foo") | literal("bar"))(Input("foo", 0)), "foo", 3)
    }
    "| falls back to the second branch when the first fails" in {
      successAt((literal("foo") | literal("bar"))(Input("bar", 0)), "bar", 3)
    }
    "| fails when neither branch matches" in {
      failsAt((literal("foo") | literal("bar"))(Input("baz", 0)), 0)
    }

    "~> drops the left value" in {
      successAt((literal("foo") ~> literal("bar"))(Input("foobar", 0)), "bar", 6)
    }
    "~> propagates failure from either side" in {
      failsAt((literal("foo") ~> literal("bar"))(Input("xfoo", 0)), 0)
      failsAt((literal("foo") ~> literal("bar"))(Input("foozzz", 0)), 3)
    }

    "<~ drops the right value" in {
      successAt((literal("foo") <~ literal("bar"))(Input("foobar", 0)), "foo", 6)
    }
    "<~ propagates failure from either side" in {
      failsAt((literal("foo") <~ literal("bar"))(Input("xfoo", 0)), 0)
      failsAt((literal("foo") <~ literal("bar"))(Input("foozzz", 0)), 3)
    }

    "^^ maps the success value" in {
      successAt((Parsers.regex("[0-9]+") ^^ (_.toInt))(Input("42", 0)), 42, 2)
    }
    "^^ propagates failure unchanged" in {
      failsAt((literal("foo") ^^ (_.toUpperCase))(Input("bar", 0)), 0)
    }

    "^^^ replaces the success value with a constant" in {
      successAt((literal("true") ^^^ 1)(Input("true", 0)), 1, 4)
    }
    "^^^ propagates failure unchanged" in {
      failsAt((literal("true") ^^^ 1)(Input("false", 0)), 0)
    }

    "operators chain left-associatively" in {
      // a ~ b ~ c parses as (a ~ b) ~ c, value type ((A, B), C)
      val p = literal("a") ~ literal("b") ~ literal("c")
      p(Input("abc", 0)) match
        case Success(((a, b), c), n) =>
          a shouldBe "a"; b shouldBe "b"; c shouldBe "c"; n.offset shouldBe 3
        case Failure(m, _) => fail(m)
    }

    "operators compose: ~> and <~ keep only the meaningful sub-result" in {
      // parens around an identifier — the parens themselves are discarded
      val p = literal("(") ~> Parsers.regex("[a-z]+") <~ literal(")")
      successAt(p(Input("(hello)", 0)), "hello", 7)
    }

    "operators compose: ^^ on a ~ result destructures the pair" in {
      // sum two digits separated by '+'
      val digit = Parsers.regex("[0-9]") ^^ (_.toInt)
      val p     = (digit <~ literal("+")) ~ digit ^^ { case (a, b) => a + b }
      successAt(p(Input("3+4", 0)), 7, 3)
    }
  }

  // ---- chainl1 / chainr1 ----

  "chainl1" - {
    // left-associative subtraction: 10 - 3 - 2 = (10 - 3) - 2 = 5
    def num: Parser[Int]                = lex(Parsers.regex("[0-9]+")) ^^ (_.toInt)
    def sub: Parser[(Int, Int) => Int]  = lex(literal("-")) ^^^ (_ - _)
    def expr                            = chainl1(num, sub)

    "folds left-to-right" in {
      successAt(expr(Input("10 - 3 - 2", 0)), 5, 10)
    }
    "single operand still works" in {
      successAt(expr(Input("42", 0)), 42, 2)
    }
    "fails when the leading operand fails" in {
      failsAt(expr(Input("xxx", 0)), 0)
    }
    "absorbs a trailing 'op without operand' (cursor stays put)" in {
      // "5 - 7 -" parses "5 - 7" and stops at the dangling "-"; parseAll
      // would then fail at end-of-input. The trailing whitespace after "7"
      // is consumed by `lex`, so the cursor sits exactly on the "-".
      expr(Input("5 - 7 -", 0)) match
        case Success(v, n) =>
          v shouldBe -2
          n.offset shouldBe 6    // points at the dangling "-"
        case Failure(m, _) => fail(m)
    }
  }

  "chainr1" - {
    // right-associative power: 2 ^ 3 ^ 2 = 2 ^ (3 ^ 2) = 2 ^ 9 = 512
    def num: Parser[Int]               = lex(Parsers.regex("[0-9]+")) ^^ (_.toInt)
    def pow: Parser[(Int, Int) => Int] = lex(literal("^")) ^^^ ((a, b) => math.pow(a.toDouble, b.toDouble).toInt)
    def expr                           = chainr1(num, pow)

    "folds right-to-left" in {
      successAt(expr(Input("2 ^ 3 ^ 2", 0)), 512, 9)
    }
    "single operand still works" in {
      successAt(expr(Input("7", 0)), 7, 1)
    }
  }

  // ---- recursive grammar (chainl1 form) ----
  // Same arithmetic grammar, written with chainl1 — no per-level fold-by-hand
  // boilerplate. Each non-terminal collapses to a single expression.

  "expression grammar (chainl1 form)" - {
    def number:    Parser[Int]                = lex(Parsers.regex("[0-9]+")) ^^ (_.toInt)
    def addOp:     Parser[(Int, Int) => Int]  =
      (lex(literal("+")) ^^^ ((_: Int) + (_: Int))) |
      (lex(literal("-")) ^^^ ((_: Int) - (_: Int)))
    def mulOp:     Parser[(Int, Int) => Int]  =
      (lex(literal("*")) ^^^ ((_: Int) * (_: Int))) |
      (lex(literal("/")) ^^^ ((_: Int) / (_: Int)))

    def factor: Parser[Int] = (in: Input) =>
      (number | (lex(literal("(")) ~> expr <~ lex(literal(")"))))(in)
    def term:   Parser[Int] = (in: Input) => chainl1(factor, mulOp)(in)
    def expr:   Parser[Int] = (in: Input) => chainl1(term,   addOp)(in)

    "parses a single number" in {
      parseAll(expr, "42") match
        case Success(v, _) => v shouldBe 42
        case Failure(m, _) => fail(m)
    }
    "evaluates 1 + 2 * 3 = 7" in {
      parseAll(expr, "1 + 2 * 3") match
        case Success(v, _) => v shouldBe 7
        case Failure(m, _) => fail(m)
    }
    "respects parens: (1 + 2) * 3 = 9" in {
      parseAll(expr, "(1 + 2) * 3") match
        case Success(v, _) => v shouldBe 9
        case Failure(m, _) => fail(m)
    }
    "left-associates subtraction: 10 - 3 - 2 = 5" in {
      parseAll(expr, "10 - 3 - 2") match
        case Success(v, _) => v shouldBe 5
        case Failure(m, _) => fail(m)
    }
    "handles a deeper expression: 2 * (3 + 4) - 1 = 13" in {
      parseAll(expr, "2 * (3 + 4) - 1") match
        case Success(v, _) => v shouldBe 13
        case Failure(m, _) => fail(m)
    }
  }

  // ---- recursive grammar (operator form) ----
  // Same expression grammar as the named-function version below, rewritten
  // with operators to show the ergonomic payoff that motivates extending
  // Sysl's operator overloading.

  "expression grammar (operator form)" - {
    def number: Parser[Int] = lex(Parsers.regex("[0-9]+")) ^^ (_.toInt)

    def factor: Parser[Int] = (in: Input) =>
      (number | (lex(literal("(")) ~> expr <~ lex(literal(")"))))(in)

    def term: Parser[Int] = (in: Input) =>
      (factor ~ rep((lex(literal("*")) | lex(literal("/"))) ~ factor))(in) match
        case Failure(m, n) => Failure(m, n)
        case Success((head, tail), endIn) =>
          var acc = head
          for (op, v) <- tail do
            acc = if op == "*" then acc * v else acc / v
          Success(acc, endIn)

    def expr: Parser[Int] = (in: Input) =>
      (term ~ rep((lex(literal("+")) | lex(literal("-"))) ~ term))(in) match
        case Failure(m, n) => Failure(m, n)
        case Success((head, tail), endIn) =>
          var acc = head
          for (op, v) <- tail do
            acc = if op == "+" then acc + v else acc - v
          Success(acc, endIn)

    "parses a single number" in {
      parseAll(expr, "42") match
        case Success(v, _) => v shouldBe 42
        case Failure(m, _) => fail(m)
    }
    "evaluates 1 + 2 * 3 = 7" in {
      parseAll(expr, "1 + 2 * 3") match
        case Success(v, _) => v shouldBe 7
        case Failure(m, _) => fail(m)
    }
    "respects parens: (1 + 2) * 3 = 9" in {
      parseAll(expr, "(1 + 2) * 3") match
        case Success(v, _) => v shouldBe 9
        case Failure(m, _) => fail(m)
    }
    "handles a deeper expression: 2 * (3 + 4) - 1 = 13" in {
      parseAll(expr, "2 * (3 + 4) - 1") match
        case Success(v, _) => v shouldBe 13
        case Failure(m, _) => fail(m)
    }
    "left-associates subtraction: 10 - 3 - 2 = 5" in {
      parseAll(expr, "10 - 3 - 2") match
        case Success(v, _) => v shouldBe 5
        case Failure(m, _) => fail(m)
    }
    "tolerates surrounding whitespace" in {
      parseAll(expr, "   1 + 2   ") match
        case Success(v, _) => v shouldBe 3
        case Failure(m, _) => fail(m)
    }
    "fails on a syntax error" in {
      parseAll(expr, "1 + ") match
        case Success(v, _) => fail(s"expected failure, got $v")
        case Failure(_, _) => succeed
    }
  }

  // ---- recursive grammar: arithmetic with + - * / and parens ----
  // Demonstrates that the closure-wrapping idiom (def p: Parser[T] = (in) => ...(in))
  // safely handles mutual recursion without lazyP. Each non-terminal returns a fresh
  // closure whose body only runs when applied to an Input.

  "expression grammar" - {
    def number: Parser[Int] = lex(map(Parsers.regex("[0-9]+"), _.toInt))

    def factor: Parser[Int] = (in: Input) =>
      or(
        number,
        seqR(lex(literal("(")), seqL(expr, lex(literal(")"))))
      )(in)

    def term: Parser[Int] = (in: Input) =>
      seq(factor, rep(seq(or(lex(literal("*")), lex(literal("/"))), factor)))(in) match
        case Failure(m, n) => Failure(m, n)
        case Success((head, tail), endIn) =>
          var acc = head
          for (op, v) <- tail do
            acc = if op == "*" then acc * v else acc / v
          Success(acc, endIn)

    def expr: Parser[Int] = (in: Input) =>
      seq(term, rep(seq(or(lex(literal("+")), lex(literal("-"))), term)))(in) match
        case Failure(m, n) => Failure(m, n)
        case Success((head, tail), endIn) =>
          var acc = head
          for (op, v) <- tail do
            acc = if op == "+" then acc + v else acc - v
          Success(acc, endIn)

    "parses a single number" in {
      parseAll(expr, "42") match
        case Success(v, _) => v shouldBe 42
        case Failure(m, _) => fail(m)
    }
    "respects precedence: 1 + 2 * 3 = 7" in {
      parseAll(expr, "1 + 2 * 3") match
        case Success(v, _) => v shouldBe 7
        case Failure(m, _) => fail(m)
    }
    "respects parens: (1 + 2) * 3 = 9" in {
      parseAll(expr, "(1 + 2) * 3") match
        case Success(v, _) => v shouldBe 9
        case Failure(m, _) => fail(m)
    }
    "handles nested parens and subtraction: 2 * (3 + 4) - 1 = 13" in {
      parseAll(expr, "2 * (3 + 4) - 1") match
        case Success(v, _) => v shouldBe 13
        case Failure(m, _) => fail(m)
    }
    "left-associates subtraction: 10 - 3 - 2 = 5" in {
      parseAll(expr, "10 - 3 - 2") match
        case Success(v, _) => v shouldBe 5
        case Failure(m, _) => fail(m)
    }
    "tolerates surrounding whitespace" in {
      parseAll(expr, "   1 + 2   ") match
        case Success(v, _) => v shouldBe 3
        case Failure(m, _) => fail(m)
    }
    "fails on a syntax error" in {
      parseAll(expr, "1 + ") match
        case Success(v, _) => fail(s"expected failure, got $v")
        case Failure(_, _) => succeed
    }
  }
