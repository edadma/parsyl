# parsyl

A parser combinator library written in [sysl](https://github.com/edadma/trisc).

This is the first sysl-native library — there is no Scala/Java/etc.
implementation. The library is the source of truth and lives entirely
in [`parsyl/parsyl.lsysl`](parsyl/parsyl.lsysl).

## Features

- Core combinators: `success`, `failure`, `eoi`, `map`, `flat_map`,
  `seq`, `seq_l`, `seq_r`, `or`, `opt`, `rep`, `rep1`, `repsep`,
  `rep1sep`, `chainl1`, `chainr1`, `accept_if`, `literal`,
  `skip_ws`, `lex`, `parse_all`.
- Operators (via user-defined `#operator` traits):
  - `~` — sequence (returns a tuple)
  - `|` — alternation
  - `~>` / `<~` — sequence keep-right / keep-left
  - `^^` — map
  - `^^^` — replace with constant
  - `!` — PEG-style negative lookahead
  - `&` — PEG-style positive lookahead
- Bare-string operands lift to `lex(literal(s))` automatically, so
  grammars read like the canonical Scala parser-combinator idiom:
  - `&"ab" ~> ("ab" <~ !"c")`
  - `("(" ~> expr <~ ")")`
- Recursive grammars via call-by-name parameters (`b: => Parser[B]`).
- Parameterless function declarations (`expr -> Parser[int] = ...`).

## Demo grammars

The same arithmetic grammar appears twice in `parsyl.lsysl`:

1. **Inline-evaluating expression parser** — folds an `int` directly,
   six function bodies covering `+ - * /`, parens, unary negation,
   left-associativity, and arbitrary whitespace.

2. **AST-building expression parser** — builds an `Expr` tree
   (`Num`/`Add`/`Sub`/`Mul`/`Div`/`Neg`/`App`) and a separate `eval`
   walks it. Includes function-call syntax `f(a, b, c)` that builds
   `App(name: string, args: []Expr)` — the variadic-children shape
   stress-tests recursive-enum slice fields.

## Running the test suite

The sysl compiler currently lives in the trisc repo. Until standalone
sysl tooling lands, run from the trisc working directory:

```
cd /path/to/trisc
sbt "syslCliJVM/run test /path/to/parsyl/parsyl/parsyl.lsysl"
```

Filter or change backend:

```
sbt "syslCliJVM/run test --filter expr_unary_neg /path/to/parsyl/parsyl/parsyl.lsysl"
sbt "syslCliJVM/run test --backend trisc /path/to/parsyl/parsyl/parsyl.lsysl"
```

## Layout

```
parsyl/
├── parsyl/
│   └── parsyl.lsysl       # the entire library + tests
├── README.md
├── LICENSE
└── .gitignore
```

## License

ISC. See [LICENSE](LICENSE).
