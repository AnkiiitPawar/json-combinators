{-# LANGUAGE BangPatterns #-}

-- | A small parser-combinator library over strict 'ByteString' input.
--
-- A parser is a function from the input and an offset to a result: either a
-- value with the offset where it stopped, or a failure at an offset with the
-- list of things that would have been accepted there.
--
-- Choice is committed, as in Parsec: @p '<|>' q@ tries @q@ only if @p@ failed
-- without consuming input. That keeps memory use flat (no input is retained
-- for backtracking) and makes error positions precise, because a parser that
-- has started to recognise something reports the failure where it happened.
-- When both alternatives fail at the same place, their expectations merge,
-- which is how messages such as "expected ',' or ']'" come about.
module JsonCombinators.Parser
  ( -- * Running parsers
    Parser
  , ParseError (..)
  , runParser
  , renderError
    -- * Primitives
  , satisfy
  , byte
  , bytes
  , peekByte
  , takeWhileP
  , takeWhile1P
  , skipWhile
  , eof
  , getOffset
  , failAt
  , label
  , (<?>)
    -- * Combinators
  , sepBy
  , sepBy1
  , between
  , option
  , choice
  ) where

import Control.Applicative (Alternative (..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Unsafe as BU
import Data.List (intercalate, nub)
import Data.Word (Word8)

-- | The result of running a parser from some offset.
data Result a
  = Ok !a !Int
  | Err !Int [String]

newtype Parser a = Parser {unParser :: B.ByteString -> Int -> Result a}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s i -> case p s i of
    Ok a j -> Ok (f a) j
    Err e x -> Err e x
  {-# INLINE fmap #-}

instance Applicative Parser where
  pure a = Parser $ \_ i -> Ok a i
  {-# INLINE pure #-}
  Parser pf <*> Parser pa = Parser $ \s i -> case pf s i of
    Err e x -> Err e x
    Ok f j -> case pa s j of
      Ok a k -> Ok (f a) k
      Err e x -> Err e x
  {-# INLINE (<*>) #-}

instance Monad Parser where
  Parser p >>= k = Parser $ \s i -> case p s i of
    Ok a j -> unParser (k a) s j
    Err e x -> Err e x
  {-# INLINE (>>=) #-}

instance Alternative Parser where
  empty = Parser $ \_ i -> Err i []
  Parser p <|> Parser q = Parser $ \s i -> case p s i of
    Err e x
      | e == i -> case q s i of
          Err e' x' | e' == i -> Err i (x ++ x')
          r -> r
    r -> r
  {-# INLINE (<|>) #-}

  -- A strict loop instead of the class's default mutual recursion of many and
  -- some. It stops at the first failure that consumed nothing, and propagates
  -- one that consumed input (the item had started, so the input is malformed).
  many (Parser p) = Parser $ \s i ->
    let go acc j = case p s j of
          Ok a k -> go (a : acc) k
          Err e x
            | e == j -> Ok (reverse acc) j
            | otherwise -> Err e x
     in go [] i
  {-# INLINE many #-}

  some p = (:) <$> p <*> many p
  {-# INLINE some #-}

-- | Where and why a parse failed. Lines and columns count from 1; columns
-- count bytes.
data ParseError = ParseError
  { errorOffset :: !Int
  , errorLine :: !Int
  , errorColumn :: !Int
  , errorExpected :: [String]
  , errorFound :: String
  }
  deriving (Eq, Show)

-- | Run a parser on the whole input or a prefix of it (add 'eof' to insist on all).
runParser :: Parser a -> B.ByteString -> Either ParseError a
runParser (Parser p) s = case p s 0 of
  Ok a _ -> Right a
  Err e x -> Left (mkError s e x)

mkError :: B.ByteString -> Int -> [String] -> ParseError
mkError s e x =
  ParseError
    { errorOffset = e
    , errorLine = 1 + B.count 10 before
    , errorColumn = 1 + B.length before - maybe 0 (+ 1) (B.elemIndexEnd 10 before)
    , errorExpected = nub x
    , errorFound = found
    }
  where
    before = B.take e s
    found
      | e >= B.length s = "end of input"
      | otherwise = describe (BU.unsafeIndex s e)

describe :: Word8 -> String
describe w
  | w >= 0x21 && w < 0x7f = ['\'', toEnum (fromIntegral w), '\'']
  | w == 0x20 = "space"
  | w == 10 = "newline"
  | otherwise = "byte 0x" ++ hex (fromIntegral w)
  where
    hex n = [digits !! (n `div` 16), digits !! (n `mod` 16)]
    digits = "0123456789abcdef"

-- | "line 3, column 14: expected ',' or ']', found 'x'"
renderError :: ParseError -> String
renderError e =
  "line " ++ show (errorLine e) ++ ", column " ++ show (errorColumn e) ++ ": " ++ what ++ "found " ++ errorFound e
  where
    what = case errorExpected e of
      [] -> ""
      xs -> "expected " ++ oxford xs ++ ", "
    oxford [a] = a
    oxford [a, b] = a ++ " or " ++ b
    oxford xs = intercalate ", " (init xs) ++ " or " ++ last xs

-- | One byte that satisfies the predicate.
satisfy :: (Word8 -> Bool) -> Parser Word8
satisfy f = Parser $ \s i ->
  if i < B.length s && f (BU.unsafeIndex s i) then Ok (BU.unsafeIndex s i) (i + 1) else Err i []
{-# INLINE satisfy #-}

-- | A given byte, described in errors as that character.
byte :: Word8 -> Parser Word8
byte w = satisfy (== w) <?> describe w
{-# INLINE byte #-}

-- | A given sequence of bytes. It consumes nothing unless the whole sequence matches.
bytes :: B.ByteString -> Parser B.ByteString
bytes t = Parser $ \s i ->
  if t `B.isPrefixOf` BU.unsafeDrop i s then Ok t (i + B.length t) else Err i [show (BC.unpack t)]

-- | The next byte, if any, without consuming it.
peekByte :: Parser (Maybe Word8)
peekByte = Parser $ \s i -> Ok (if i < B.length s then Just (BU.unsafeIndex s i) else Nothing) i
{-# INLINE peekByte #-}

-- | The longest run of bytes satisfying the predicate (possibly empty), as a slice of the input.
takeWhileP :: (Word8 -> Bool) -> Parser B.ByteString
takeWhileP f = Parser $ \s i ->
  let run = B.takeWhile f (BU.unsafeDrop i s) in Ok run (i + B.length run)
{-# INLINE takeWhileP #-}

-- | Like 'takeWhileP' but at least one byte.
takeWhile1P :: (Word8 -> Bool) -> Parser B.ByteString
takeWhile1P f = Parser $ \s i ->
  let run = B.takeWhile f (BU.unsafeDrop i s)
   in if B.null run then Err i [] else Ok run (i + B.length run)
{-# INLINE takeWhile1P #-}

skipWhile :: (Word8 -> Bool) -> Parser ()
skipWhile f = Parser $ \s i ->
  let go !j = if j < B.length s && f (BU.unsafeIndex s j) then go (j + 1) else j in Ok () (go i)
{-# INLINE skipWhile #-}

eof :: Parser ()
eof = Parser $ \s i -> if i >= B.length s then Ok () i else Err i ["end of input"]

getOffset :: Parser Int
getOffset = Parser $ \_ i -> Ok i i

-- | Fail at a given offset (earlier than the current one, to point at the
-- start of the offending token) with a message.
failAt :: Int -> String -> Parser a
failAt e msg = Parser $ \_ _ -> Err e [msg]

-- | Name what a parser recognises, for errors where it fails without consuming input.
label :: String -> Parser a -> Parser a
label name (Parser p) = Parser $ \s i -> case p s i of
  Err e _ | e == i -> Err e [name]
  r -> r
{-# INLINE label #-}

infix 0 <?>

(<?>) :: Parser a -> String -> Parser a
p <?> name = label name p
{-# INLINE (<?>) #-}

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = open *> p <* close

option :: a -> Parser a -> Parser a
option a p = p <|> pure a

choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty
