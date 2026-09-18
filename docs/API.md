# API guide

Use qualified imports in examples below. The contracts describe this checkout,
including intentional fixes listed in the changelog. Functions not documented
as safe should not be assumed total.

## Lists, text and helpers

`List.safe_head`, `safe_tail` and `safe_nth` return `Maybe`. `safe_nth` is
**one-based**, returning `Nothing` for non-positive or out-of-range indices.
`Misc.!!?` is **zero-based**. `List.indexOf` returns the list length if the value
is absent; use `safe_indexOf` when absence must be distinguished.

`List.flatten` accepts empty and singleton outer lists. `reduce` accepts a
singleton but still throws on an empty list. `splitEach` requires a positive
chunk size. `splitWhen` requires a non-empty delimiter and preserves empty
segments. `replaceStr "" replacement input` returns the input unchanged.
`transpose` follows `Data.List.transpose`, including empty and ragged inputs.
It does **not** pad missing cells; validate rectangular tables before using it
for row/column alignment.

Other splitting helpers are not aliases: `split` drops a trailing empty segment,
while `splitOn` skips leading separators but can keep subsequent empty segments.
`groupBy` groups matching keys across the whole input, rather than only adjacent
runs like `Data.List.groupBy`. `quickSortBy` is not stable. `uniq` preserves the
first occurrences, with quadratic worst-case cost.

`showTableWithHeaders` takes **columns**, not rows, and expects non-empty columns
of equal height. `nice'title` expects non-empty text. `basename` is partial for
empty/root-only paths. Prefer `System.FilePath` when implementing new file tools;
`Paths.</>` only joins slash strings and does not reset at an absolute right side.

`Tuple.toTuple2` and `toTuple3` require exactly two/three elements. `Misc.fromJust`
and `fromEither` throw; use pattern matching for recoverable failure.
`Misc.isLetter`, `isNumber` and `isSpace` use custom ASCII character sets.
`Trees.Tree` traverses in preorder; its `Read` format is not its displayed tree.

## Fractions and statistics

```haskell
Maths.readFraction "3/4"          -- 3/4
Maths.fromFraction (3 Maths.% 4) :: Double  -- 0.75
Stats.µ [1,2,3 :: Rational]       -- 2 % 1
Stats.σ² [1,2,3 :: Rational]      -- 2 % 3, population variance
Stats.s² [1,2,3 :: Rational]      -- 1 % 1, sample variance
```

`Maths.Fraction` uses `Integer` fields. Its constructor is exposed; there is no
invariant preventing a zero denominator. `readFraction` is partial; even its
`Read` instance can throw while trying to parse invalid input. Decimal/scientific
notation is not generally interchangeable with `read :: String -> Double`.
`fromFraction` can lose precision when the destination is floating point.
`roundFraction`, `fracPower` and formatted output also use floating arithmetic.
`fact` requires a non-negative argument. `pgcd` does not handle a zero second
argument. `prettyBytes` historically labels its input as kB and divides by 1024;
it is not a reliable byte-unit formatter. `sumOfDigits` repeatedly sums digits
until a single digit remains (for example, 99 becomes 9, not 18).

`median` sorts its input, averages the middle pair for an even count and throws
on an empty list. `choose n k` returns zero for `k < 0` or `k > n`, and throws for
negative `n`. Use `Integer` to avoid intermediate overflow. `binom`/`dbinom`
expect `0 <= p <= 1` and non-negative trial counts; they do not validate them.

Means and population variance require at least one observation; sample variance
requires at least two. `cov`, `r` and `r²` require paired vectors of equal length;
the implementation truncates pairs if you violate this. Correlation additionally
requires non-zero variance in both vectors.

`f_stat_critical p df1 df2` currently takes a **lower-tail cumulative probability**:
use `0.95` for the usual upper 5% threshold. It returns `Just` unconditionally
and does not reliably use `Nothing` for invalid input. `oneWay_anova` uses that
same probability convention; its argument name alpha is misleading. The stored
`Distribution_Tables.alpha` uses the *upper-tail* convention. Do not mix them.
`cloppearson` has not been validated as a Clopper–Pearson interval, and `qnorm`
is `undefined`. These are outstanding issues, not supported statistical APIs.

Random helpers use explicit `State` generators. `sample k population` samples
without replacement and requires `0 <= k <= length population`. `samples n k`
reuses the original population for each sample and requires `n >= 0`.
`normalr` still has a possible `log 0` endpoint. See the review before using
these routines in analysis that depends on numerical correctness.

## CSV, data frames and matrices

```haskell
CSV.parseCSV ';' "name;note\r\nAlice;\"a;b\"\r\n"
-- [["name","note"],["Alice","a;b"]]
```

`CSV` is `[[String]]`, with rows outside and cells inside. Quoted delimiters,
doubled quotes and embedded newlines are supported. A row with exactly one
empty cell is discarded, including a quoted empty cell. Unterminated quotes
throw. `guessSep` is a heuristic for comma/semicolon using raw counts; quoting
can confuse it. There is no streaming parser, validation result or CSV writer.

`DF.csv'to'DF True` consumes the first row as titles; it requires a non-empty
input. With `False`, titles are generated as `"1"`, `"2"`, etc. All columns start
as `String'`; `DF.types` changes metadata only, without validating cell values.
Numeric summaries silently discard values that `readMaybe` cannot parse.
`DF.readXlsx` loads `Sheet1`; missing sheets and malformed/empty workbooks can
fail. `wsRange` and `cellToString` have partial cases. Large sparse cell ranges
may be expanded into large dense tables.

`Matrix.Matrix rows columns cells` uses flat, row-major storage. `rowN 1` returns
the first row. The public constructor permits inconsistent dimensions;
`elementwise` can silently truncate different shapes. `readMatrix` does not
properly validate rectangular rows. `toMatrix` is a separate legacy column
wrapper whose stored dimensions do not follow the usual cell-count invariant.

## JSON and HTTP

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Aeson as Aeson
import qualified Json

name = Json.lookupMaybe "name" (Aeson.object ["name" Aeson..= ("Alice" :: String)])
-- Just (Aeson.String "Alice")
```

`lookupMaybe` returns `Nothing` for a missing field or non-object value;
`unStringMaybe` does the same for non-string values. The old `lookup` and
`unString` are retained and partial.

`Requester.build GET url body` ignores `body`; `POST` preserves it. Building
parses a request but does not contact the server. HTTP/network exceptions are
not caught. For multiple requests, bind `m <- Requester.manager` once and call
`Requester.sendWith m request`. `send` still creates its own manager each time.
Both functions return a legacy Char8 string; they do not perform UTF-8 decoding
or interpret the response content type. Use `httpLbs` directly when status,
headers, encoding or large/streamed bodies matter.

## Rendering and encodings

`Html.Text` and attribute values are HTML-escaped. Existing strings containing
entities will be escaped again. Tag/attribute names are unrestricted, dangerous
URL schemes are not filtered, and `script`/`style` contexts need different rules.
`Show Element` also inserts spaces and closes void elements; it is a small helper,
not a conforming general HTML renderer.

`Colors.Color` stores fractional RGB channels, conventionally in `[0,1]`.
`color'toHex`/`color'toRgb` assume valid channels. `Read Color` and `fromList`
are partial. `SVG_creator` stores raw colour strings; validate six hexadecimal
digits before constructing a colour. Its output is for trusted input.

`Pixels.pixelsToPng destination width height groups` expects groups of `[x,y]`
points, rounds coordinates, colours at most six groups and paints the rest white.
Provide positive dimensions. It scans all points per pixel.

`Kmeans.kmeans k points` seeds from the first `k` points and uses Euclidean
coordinates. It requires `1 <= k <= length points`, finite coordinates and
non-empty points of equal dimension. Empty clusters retain their previous
centre. Termination uses movement below **one coordinate unit** for every
centre; there is no iteration limit, scale adaptation or k-means++ seeding.
Finite but huge coordinates can still overflow intermediate floating operations.
`clusterize` and `euclidist` themselves remain unchecked.

`Bases.to'base`/`from'base` work best with non-negative `Int` values and bases
2–16; output hex is uppercase. Readers do not reject every invalid digit;
negative numbers and invalid bases can diverge. Base 64 is unimplemented.
`QuotedPrintable.encode` has known UTF-8 boundary and newline-handling defects;
it is not suitable as a standards-compliant mail encoder yet.

`Smiley.parseSmiley mappings text` is the portable pure entry point. Mapping
keys must be non-empty to ensure progress. The boundary alphabet is custom
ASCII, so Unicode word boundaries need further work. The convenience IO loaders
still refer to a personal CSV file.
