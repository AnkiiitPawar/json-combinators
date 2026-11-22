{-# LANGUAGE BangPatterns #-}

-- | JSON (RFC 8259) parsed with the combinators of "JsonCombinators.Parser".
--
-- Numbers are kept exactly, as a decimal coefficient and exponent, so
-- 123456789012345678901234567890 and 0.1 survive a round trip unchanged.
-- Strings are UTF-8 checked, escapes are decoded (surrogate pairs included),
-- object members keep their order, and nesting is limited to 'maxDepth'.
module JsonCombinators
  ( Value (..)
  , Decimal
  , decimal
  , coefficient
  , exponent10
  , toDouble
  , decode
  , decodeWith
  , maxDepth
  , value
  , encode
  , encodePretty
  , ParseError (..)
  , renderError
  ) where

import Control.Applicative (many, (<|>))
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Bits (shiftL, (.|.))
import Data.Char (chr, ord)
import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import JsonCombinators.Parser

data Value
  = Null
  | Bool !Bool
  | Number !Decimal
  | String !T.Text
  | Array [Value]
  | Object [(T.Text, Value)]
  deriving (Eq, Show)

-- | coefficient * 10 ^ exponent, with the trailing zeros of the coefficient
-- moved into the exponent (and zero stored as 0e0), so that numerically equal
-- numbers are equal values. The sign of a negative zero is not kept.
data Decimal = Decimal !Integer !Integer
  deriving (Eq, Show)

decimal :: Integer -> Integer -> Decimal
decimal 0 _ = Decimal 0 0
decimal c e
  | c `rem` 10 == 0 = decimal (c `quot` 10) (e + 1)
  | otherwise = Decimal c e

coefficient :: Decimal -> Integer
coefficient (Decimal c _) = c

exponent10 :: Decimal -> Integer
exponent10 (Decimal _ e) = e

-- | The nearest Double, with overflow to infinity and underflow to zero.
toDouble :: Decimal -> Double
toDouble (Decimal c e)
  | c == 0 = 0
  | magnitude > 400 = if c > 0 then 1 / 0 else -1 / 0
  | magnitude < -400 = if c > 0 then 0 else -0
  | otherwise = fromRational (fromInteger c * 10 ^^ e) -- exact, then correctly rounded
  where
    magnitude = e + fromIntegral (length (show (abs c)))

-- | Deepest nesting of arrays and objects accepted by 'decode'.
maxDepth :: Int
maxDepth = 512

-- | Parse a complete JSON text: one value, surrounded by optional whitespace.
decode :: B.ByteString -> Either ParseError Value
decode = decodeWith maxDepth

decodeWith :: Int -> B.ByteString -> Either ParseError Value
decodeWith depth = runParser (ws *> value depth <* ws <* eof)

ws :: Parser ()
ws = skipWhile (\w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09)
{-# INLINE ws #-}

-- | A value, allowing `depth` more levels of nesting. The first byte decides
-- which kind of value follows, so no alternative ever needs to back up.
value :: Int -> Parser Value
value depth = do
  next <- peekByte
  case next of
    Just 0x7b -> container (object depth)
    Just 0x5b -> container (array depth)
    Just 0x22 -> String <$> string
    Just 0x74 -> Bool True <$ bytes kTrue
    Just 0x66 -> Bool False <$ bytes kFalse
    Just 0x6e -> Null <$ bytes kNull
    Just w | w == 0x2d || isDigit w -> Number <$> number
    _ -> failHere "a JSON value"
  where
    container p
      | depth <= 0 = do
          at <- getOffset
          failAt at ("nesting no deeper than " ++ show maxDepth ++ " levels")
      | otherwise = p

kTrue, kFalse, kNull :: B.ByteString
kTrue = BC.pack "true"
kFalse = BC.pack "false"
kNull = BC.pack "null"

failHere :: String -> Parser a
failHere what = getOffset >>= \at -> failAt at what

-- | Opening bracket, then either the closing bracket at once or items
-- separated by commas. Peeking for the empty case keeps the error after an
-- opening bracket specific: "expected a JSON value", not "expected ']'".
items :: Word8 -> Word8 -> String -> Parser a -> Parser [a]
items open close closing item = do
  _ <- byte open
  ws
  next <- peekByte
  if next == Just close
    then [] <$ byte close
    else do
      first <- item
      rest <- many (byte 0x2c *> ws *> item)
      _ <- byte close <?> closing
      pure (first : rest)

array :: Int -> Parser Value
array depth = Array <$> items 0x5b 0x5d "',' or ']'" (value (depth - 1) <* ws)

object :: Int -> Parser Value
object depth = Object <$> items 0x7b 0x7d "',' or '}'" member
  where
    member = do
      k <- string <?> "a string key"
      ws
      _ <- byte 0x3a <?> "':'"
      ws
      v <- value (depth - 1)
      ws
      pure (k, v)

isDigit :: Word8 -> Bool
isDigit w = w >= 0x30 && w <= 0x39
{-# INLINE isDigit #-}

-- | number = [ "-" ] int [ frac ] [ exp ], with no leading zeros in int.
number :: Parser Decimal
number = do
  start <- getOffset
  neg <- option False (True <$ byte 0x2d)
  intPart <- takeWhile1P isDigit <?> "a digit"
  if B.length intPart > 1 && B.head intPart == 0x30
    then failAt start "a number without leading zeros"
    else pure ()
  frac <- option B.empty (byte 0x2e *> (takeWhile1P isDigit <?> "a digit after '.'"))
  ex <- option 0 $ do
    _ <- satisfy (\w -> w == 0x65 || w == 0x45)
    sign <- option 1 ((1 <$ byte 0x2b) <|> (-1 <$ byte 0x2d))
    digits <- takeWhile1P isDigit <?> "a digit in the exponent"
    pure (sign * digitsToInteger digits)
  let c = digitsToInteger (intPart <> frac)
  pure (decimal (if neg then negate c else c) (ex - fromIntegral (B.length frac)))

-- | Decimal digits to an Integer, splitting long runs in half so that very
-- long numbers cost O(n log^2 n) rather than O(n^2).
digitsToInteger :: B.ByteString -> Integer
digitsToInteger d
  | B.length d <= 18 = fromIntegral (B.foldl' (\acc w -> acc * 10 + fromIntegral (w - 0x30)) (0 :: Int) d)
  | otherwise =
      let k = B.length d `div` 2
       in digitsToInteger (B.take k d) * 10 ^ (B.length d - k) + digitsToInteger (B.drop k d)

-- | A string: raw runs of unescaped bytes (checked as UTF-8) and escapes.
string :: Parser T.Text
string = byte 0x22 *> go []
  where
    go acc = do
      start <- getOffset
      run <- takeWhileP (\w -> w /= 0x22 && w /= 0x5c && w >= 0x20)
      chunk <-
        if B.all (< 0x80) run
          then pure (TE.decodeLatin1 run) -- ASCII: nothing to check
          else case TE.decodeUtf8' run of
            Right t -> pure t
            Left _ -> failAt start "valid UTF-8"
      next <- peekByte
      case next of
        Just 0x22 -> byte 0x22 >> pure (T.concat (reverse (chunk : acc)))
        Just 0x5c -> do
          c <- escape
          go (T.singleton c : chunk : acc)
        Just _ -> failHere "a character (control characters must be escaped)"
        Nothing -> failHere "'\"' to close the string"

escape :: Parser Char
escape = do
  start <- getOffset
  _ <- byte 0x5c
  w <- satisfy (const True) <?> "an escape character"
  case w of
    0x22 -> pure '"'
    0x5c -> pure '\\'
    0x2f -> pure '/'
    0x62 -> pure '\b'
    0x66 -> pure '\f'
    0x6e -> pure '\n'
    0x72 -> pure '\r'
    0x74 -> pure '\t'
    0x75 -> do
      hi <- hex4
      if hi >= 0xd800 && hi <= 0xdbff
        then do
          -- A high surrogate must be followed by an escaped low surrogate.
          lo <- (bytes (BC.pack "\\u") *> hex4) <|> failAt start "a low surrogate after a high one"
          if lo >= 0xdc00 && lo <= 0xdfff
            then pure (chr (0x10000 + ((hi - 0xd800) `shiftL` 10) + (lo - 0xdc00)))
            else failAt start "a low surrogate after a high one"
        else
          if hi >= 0xdc00 && hi <= 0xdfff
            then failAt start "a high surrogate before a low one"
            else pure (chr hi)
    _ -> failAt start "one of \\\" \\\\ \\/ \\b \\f \\n \\r \\t \\u"

hex4 :: Parser Int
hex4 = do
  ds <- sequenceA (replicate 4 (satisfy isHex <?> "a hexadecimal digit"))
  pure (foldl' (\acc w -> acc * 16 + hexValue w) 0 ds)
  where
    isHex w = isDigit w || (w .|. 0x20) >= 0x61 && (w .|. 0x20) <= 0x66
    hexValue w
      | isDigit w = fromIntegral (w - 0x30)
      | otherwise = fromIntegral ((w .|. 0x20) - 0x61 + 10)

-- | Compact JSON text.
encode :: Value -> B.ByteString
encode = BL.toStrict . BB.toLazyByteString . compact

compact :: Value -> BB.Builder
compact Null = BB.string7 "null"
compact (Bool b) = BB.string7 (if b then "true" else "false")
compact (Number d) = renderDecimal d
compact (String t) = quoted t
compact (Array xs) = BB.char7 '[' <> commaSep (map compact xs) <> BB.char7 ']'
compact (Object kvs) =
  BB.char7 '{' <> commaSep [quoted k <> BB.char7 ':' <> compact v | (k, v) <- kvs] <> BB.char7 '}'

commaSep :: [BB.Builder] -> BB.Builder
commaSep [] = mempty
commaSep (b : bs) = b <> mconcat [BB.char7 ',' <> x | x <- bs]

-- | Indented JSON text, two spaces per level.
encodePretty :: Value -> B.ByteString
encodePretty = BL.toStrict . BB.toLazyByteString . (<> BB.char7 '\n') . pretty 0

pretty :: Int -> Value -> BB.Builder
pretty n (Array xs@(_ : _)) = block n '[' ']' (map (pretty (n + 1)) xs)
pretty n (Object kvs@(_ : _)) =
  block n '{' '}' [quoted k <> BB.string7 ": " <> pretty (n + 1) v | (k, v) <- kvs]
pretty _ v = compact v

block :: Int -> Char -> Char -> [BB.Builder] -> BB.Builder
block n open close xs =
  BB.char7 open
    <> mconcat [sep <> BB.char7 '\n' <> indent (n + 1) <> item | (sep, item) <- zip (mempty : repeat (BB.char7 ',')) xs]
    <> BB.char7 '\n'
    <> indent n
    <> BB.char7 close
  where
    indent k = BB.string7 (replicate (2 * k) ' ')

-- | A string literal: runs of characters that need no escape are copied as
-- UTF-8 in one piece; the rest are escaped one at a time.
quoted :: T.Text -> BB.Builder
quoted t = BB.char7 '"' <> go t <> BB.char7 '"'
  where
    go s =
      let (plain, rest) = T.break needsEscape s
       in TE.encodeUtf8Builder plain <> maybe mempty (\(c, more) -> escapeChar c <> go more) (T.uncons rest)
    needsEscape c = c == '"' || c == '\\' || c < '\x20'

escapeChar :: Char -> BB.Builder
escapeChar c = case c of
  '"' -> BB.string7 "\\\""
  '\\' -> BB.string7 "\\\\"
  '\n' -> BB.string7 "\\n"
  '\r' -> BB.string7 "\\r"
  '\t' -> BB.string7 "\\t"
  '\b' -> BB.string7 "\\b"
  '\f' -> BB.string7 "\\f"
  _
    | ord c < 0x20 -> BB.string7 "\\u00" <> BB.word8HexFixed (fromIntegral (ord c))
    | otherwise -> BB.charUtf8 c

-- | Shortest-looking exact form: plain digits for moderate magnitudes,
-- scientific notation otherwise (thresholds as in JavaScript's Number#toString).
renderDecimal :: Decimal -> BB.Builder
renderDecimal (Decimal c 0) = BB.integerDec c -- the common case: an integer
renderDecimal (Decimal c e) = BB.string7 (sign ++ body)
  where
    sign = if c < 0 then "-" else ""
    ds = show (abs c)
    n = length ds
    point = fromIntegral n + e -- position of the decimal point after the first digit group
    body
      | e >= 0 && point <= 21 = ds ++ replicate (fromIntegral e) '0'
      | e < 0 && point > 0 = let (a, b) = splitAt (fromIntegral point) ds in a ++ "." ++ b
      | e < 0 && point > -6 = "0." ++ replicate (fromIntegral (negate point)) '0' ++ ds
      | otherwise = take 1 ds ++ (if n > 1 then "." ++ drop 1 ds else "") ++ "e" ++ show (point - 1)
