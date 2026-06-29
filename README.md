# Liberty Haskell Parser

A small Haskell library for the **Liberty** (`.lib`) standard-cell timing
format: a parser, an abstract syntax tree, and a serialiser that writes the AST
back out as Liberty text.

## Pipeline

```
ByteString  --Alex lexer-->  Token stream  --Happy parser-->  [Liberty] AST  --Show-->  .lib text
```

The lexer and parser are coupled and run in a single streaming pass: the Happy
parser pulls one token at a time from the Alex lexer (via its `Alex` monad),
rather than building an intermediate token list. The `Show` instances on the
AST types *are* the serialiser, so a parse followed by `show` round-trips a file
back to Liberty syntax.

## Module map

| Module | Role |
|--------|------|
| `Language.Liberty` | Umbrella / public entry point — re-exports the parser and the AST. |
| `Language.Liberty.Parser` | Happy parser; exposes `parseLiberty :: ByteString -> Either String [Liberty]`. Source is `src/Language/Liberty/Parser.y`. |
| `Language.Liberty.Lex` | Alex lexer (`monad-bytestring` wrapper) and the `Token` type. Source is `src/Language/Liberty/Lex.x`. |
| `Language.Liberty.AST` | The `Liberty` and `AttrVal` types and their serialising `Show` instances. |

The generated `Lex.hs` / `Parse.hs` should never be edited directly — edit the
`.x` / `.y` sources. Likewise the build metadata lives in `package.yaml` (hpack);
`liberty.cabal` is generated from it.

## Consumed by

This library is used by **hsncl**'s `genLiberateTemplate`, where it is imported
as `Language.Liberty` to read and emit Liberty cell templates.

## Building

Built with **Stack** (hpack-generated cabal). From this directory:

```
stack build
stack haddock --fast --no-haddock-deps   # build the API docs
stack test                               # currently a stub
```

Stack auto-detects the `alex` and `happy` build tools; they are not declared
explicitly in `package.yaml`.

## Caveats / known quirks

These are documented in the source and listed here for visibility. They are
intentional behaviour notes, **not** a to-do list:

- **The serialiser is not escape-safe.** `AST.scapeString` (an escaping helper,
  with "scape" a misspelling of "escape") is defined but never called, so
  `show (StrVal a)` emits string payloads verbatim. Quoted strings round-trip
  only because the lexer keeps their surrounding quote bytes inside the token.
- **Strings and barewords are indistinguishable in the AST.** Both quoted
  strings and identifiers become `StrVal`, so the AST does not record which
  surface syntax was used.
- **Numbers are single-precision `Float`** (`TNum Float` / `NumVal Float`), so
  high-precision values may lose precision and not byte-match the input on
  re-emit.
- **Dependency notes:** `text` is declared in `package.yaml` but unused; `array`
  is only needed transitively by Happy's generated tables.
- **No real test suite:** `test/Spec.hs` is a stub that prints
  "Test suite not yet implemented".
