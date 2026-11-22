# json-combinators

A parser-combinator library written from scratch in Haskell, and an RFC 8259 JSON parser and encoder built with it. It passes every must-accept and must-reject case of JSONTestSuite. Numbers are kept exactly. Errors say where they happened and what was expected:

```text
line 2, column 4: expected ',' or ']', found 'x'
```

## Results

| Claim | Evidence | Grade |
|---|---|---|
| Conforms to RFC 8259 on the standard test suite | JSONTestSuite (Seriot): all 95 `y_` files accepted, all 188 `n_` files rejected | proven by test |
| Encoding and decoding are inverse | `decode . encode = id` and `decode . encodePretty = id` hold on random documents with nested containers, escapes, control characters, astral-plane Unicode and 40-digit numbers | 1,000 QuickCheck cases each |
| The parser is strict about structure | Every proper prefix of an encoded array or object is rejected, and random whitespace between tokens changes nothing | 1,000 QuickCheck cases each |
| Numbers are exact | 123456789012345678901234567890, `0.1` and `1E400` round-trip unchanged. `toDouble` agrees with GHC's correctly rounded `read` | tests, plus 1,000 QuickCheck cases |
| Throughput | 41–42 MB/s decoding and 52–54 MB/s encoding a 21 MB document. Python 3.13's `json` (C extension) does 59–61 and 90–92 MB/s on the same bytes | measured, 3 runs each |

## The combinator library

`JsonCombinators.Parser` is about 230 lines, comments included. A parser is a function from the input and an offset to a result:

```haskell
newtype Parser a = Parser {unParser :: ByteString -> Int -> Result a}
data Result a = Ok !a !Int | Err !Int [String]
```

There are three design choices.

**Committed choice.** In `p <|> q`, `q` runs only if `p` failed *without consuming input*, as in Parsec. Once a parser has recognised the start of something, its failure is final. That has two effects:

- no input is held for backtracking;
- errors point at the byte where the input went wrong, not at the start of the last alternative.

**Merged expectations.** When both sides of `<|>` fail at the same offset, their lists of expected items are joined. `label` (`<?>`) names a parser for errors at its first byte. That is how a message like `expected ',' or ']'` is assembled.

**Strict results.** `Ok` is strict in the value. With a lazy value field, every `fmap` and `<*>` left a thunk in the result tree, and decoding ran at 23 MB/s. With the strict field it runs at 42 MB/s, on the same document and machine.

The primitives (`satisfy`, `byte`, `bytes`, `takeWhileP`, `skipWhile`, `peekByte`, `eof`) work on byte offsets and slice the input without copying. `many` is a strict loop rather than the class default.

## JSON with it

The grammar is LL(1): the first byte of a value decides its kind (`{`, `[`, `"`, `t`, `f`, `n`, `-` or a digit), so no alternative ever has to back up. Arrays and objects peek once after the opening bracket. Doing so keeps the empty case from muddying the error after `[`.

- **Numbers** follow the RFC grammar exactly: no leading zeros, digits required after `.` and in the exponent. They are stored as an exact decimal, coefficient × 10^exponent, with trailing zeros moved into the exponent. So `1.50 == 1.5`, and a 30-digit integer stays a 30-digit integer. Long digit strings are converted by splitting in half, not digit by digit.
- **Strings.** Runs of plain bytes are checked as UTF-8 in one call, with a fast path for pure ASCII. Escapes are decoded, and a `😀` surrogate pair becomes U+1F600. Lone surrogates, unescaped control characters and invalid UTF-8 are rejected.
- **Nesting** is limited to 512 levels. A run of 100,000 `[` fails at the 513th, rather than recursing.
- **Encoding.** Compact output copies safe runs of a string in one piece. Numbers are written as plain digits for moderate magnitudes and in scientific notation beyond, with the same thresholds as JavaScript (`1.5`, `0.000001`, `1e-7`, `1.23e22`). `encodePretty` indents by two spaces.

Examples of error messages, all from the test suite:

```text
{"a": 1,}        line 1, column 9: expected a string key, found '}'
[tru]            line 1, column 2: expected "true", found 't'
{"a" 1}          line 1, column 6: expected ':', found '1'
01               line 1, column 1: expected a number without leading zeros, found '0'
"a<TAB>b"        line 1, column 3: expected a character (control characters must be escaped), found byte 0x09
["\uDC00"]       line 1, column 3: expected a high surrogate before a low one, found '\'
```

### Implementation-defined cases

JSONTestSuite has 35 `i_` files where RFC 8259 lets parsers choose. This parser:

- **accepts 11:** all ten number edge cases, such as `-1e+9999`, `123e-10000000` and a 48-digit negative integer (all kept exactly), and 500 levels of nesting;
- **rejects 24:** every string that is not valid UTF-8 or contains a lone surrogate, UTF-16 input, and a leading byte-order mark.

The full list is printed by the test suite.

## Usage

```haskell
import qualified Data.ByteString.Char8 as BC
import JsonCombinators

main :: IO ()
main = case decode (BC.pack "{\"pi\": 3.14159, \"tags\": [\"a\", \"b\"]}") of
  Left err -> putStrLn (renderError err)
  Right v -> BC.putStr (encodePretty v)
```

```bash
cabal build
cabal test --test-show-details=direct    # properties, JSONTestSuite, examples: under a second
cabal run throughput && python3 scripts/bench_python.py
```

The benchmark figures come from GHC 9.4.7 at `-O2` on a 2-vCPU Xeon at 2.1 GHz. The document is 110,000 records with integers, decimals, booleans, nested arrays and objects, and strings with escapes and non-ASCII text. Each figure is the best of five runs.

## Layout

```text
src/JsonCombinators/Parser.hs   the combinator library
src/JsonCombinators.hs          JSON values, parser, encoder, pretty printer
test/Main.hs                    QuickCheck properties, JSONTestSuite runner, examples
bench/Main.hs                   throughput benchmark (writes bench-doc.json)
scripts/bench_python.py         Python's json on the same document
tests/JSONTestSuite/            test files and licence (MIT, Nicolas Seriot)
```

## Limits

- **Whole input only.** Input is a strict `ByteString`, so there is no streaming or incremental parsing.
- **Duplicate keys** are all kept, in order. The RFC leaves their meaning open.
- **Negative zero** is read as zero, because the exact decimal has no sign for it.
- **Error hints.** Errors carry the expectations of the parser that failed, not hints from earlier alternatives that succeeded without consuming input. So a few messages are less specific than Parsec's would be.

## References

- T. Bray (ed.), *The JavaScript Object Notation (JSON) Data Interchange Format*, RFC 8259 (2017).
- N. Seriot, *Parsing JSON is a Minefield* and JSONTestSuite, github.com/nst/JSONTestSuite (test files from commit 1ef36fa).
- D. Leijen, E. Meijer, *Parsec: Direct Style Monadic Parser Combinators For The Real World*, Technical Report UU-CS-2001-27, Utrecht University (2001).
- G. Hutton, E. Meijer, *Monadic parsing in Haskell*, J. Funct. Program. 8, 437 (1998).
- K. Claessen, J. Hughes, *QuickCheck: a lightweight tool for random testing of Haskell programs*, ICFP (2000).

## Licence

MIT. The files in `tests/JSONTestSuite/` are MIT-licensed by Nicolas Seriot; see the licence file there.
