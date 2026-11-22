-- | Properties checked with QuickCheck, the JSONTestSuite conformance files,
-- and examples of exact numbers, Unicode and error messages.
module Main (main) where

import Control.Monad (forM, unless, when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Either (isLeft, isRight)
import Data.IORef
import Data.List (isPrefixOf, sort)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import JsonCombinators
import System.Directory (listDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import Test.QuickCheck

-- Generators ---------------------------------------------------------------

newtype J = J Value deriving (Show)

instance Arbitrary J where
  arbitrary = J <$> sized genValue
  shrink (J v) = map J (shrinkValue v)

genValue :: Int -> Gen Value
genValue n
  | n <= 1 = genLeaf
  | otherwise =
      frequency
        [ (3, genLeaf)
        , (1, Array <$> few (genValue (n `div` 3)))
        , (1, Object <$> few ((,) <$> genText <*> genValue (n `div` 3)))
        ]
  where
    few g = choose (0, 5) >>= \k -> vectorOf k g

genLeaf :: Gen Value
genLeaf = oneof [pure Null, Bool <$> arbitrary, Number <$> genDecimal, String <$> genText]

genDecimal :: Gen Decimal
genDecimal = decimal <$> oneof [arbitrary, choose (-10 ^ (40 :: Int), 10 ^ (40 :: Int))] <*> choose (-40, 40)

-- | Letters, the characters JSON must escape, control characters, and any
-- Unicode scalar value (surrogates excluded: Text cannot hold them).
genText :: Gen T.Text
genText = T.pack <$> listOf genChar
  where
    genChar =
      frequency
        [ (6, choose ('a', 'z'))
        , (1, elements "\"\\/\b\f\n\r\t")
        , (1, choose ('\0', '\x1f'))
        , (2, arbitraryUnicodeChar `suchThat` (\c -> c < '\xd800' || c > '\xdfff'))
        ]

shrinkValue :: Value -> [Value]
shrinkValue (Array xs) = xs ++ map Array (shrinkList shrinkValue xs)
shrinkValue (Object kvs) = map snd kvs ++ map Object (shrinkList (\(k, v) -> [(k, v') | v' <- shrinkValue v]) kvs)
shrinkValue (String t) = [String (T.pack s) | s <- shrink (T.unpack t)]
shrinkValue _ = []

-- | The compact encoding with random whitespace between tokens.
spaced :: Value -> Gen BB.Builder
spaced v = case v of
  Array xs -> wrap '[' ']' =<< mapM spaced xs
  Object kvs -> wrap '{' '}' =<< mapM member kvs
  _ -> pure (BB.byteString (encode v))
  where
    member (k, x) = do
      s1 <- space
      s2 <- space
      body <- spaced x
      pure (BB.byteString (encode (String k)) <> s1 <> BB.char7 ':' <> s2 <> body)
    wrap open close items = do
      gaps <- vectorOf (length items + 1) space
      let joined = mconcat [g <> (if i > 0 then BB.char7 ',' else mempty) <> h | (i, g, h) <- zip3 [0 :: Int ..] gaps items]
      pure (BB.char7 open <> joined <> last gaps <> BB.char7 close)
    space = BB.string7 <$> listOf (elements " \t\n\r")

-- Properties ---------------------------------------------------------------

prop_roundTrip :: J -> Property
prop_roundTrip (J v) = decode (encode v) === Right v

prop_prettyRoundTrip :: J -> Property
prop_prettyRoundTrip (J v) = decode (encodePretty v) === Right v

prop_encodeIdempotent :: J -> Property
prop_encodeIdempotent (J v) = (encode <$> decode (encode v)) === Right (encode v)

prop_whitespace :: J -> Property
prop_whitespace (J v) = forAll (spaced v) $ \b ->
  decode (BL.toStrict (BB.toLazyByteString b)) === Right v

-- | Cutting an array or object short anywhere leaves invalid JSON.
prop_prefixesFail :: J -> Property
prop_prefixesFail (J v) =
  isContainer v ==>
    let s = encode v in conjoin [counterexample (BC.unpack (B.take k s)) (isLeft (decode (B.take k s))) | k <- [0 .. B.length s - 1]]
  where
    isContainer (Array _) = True
    isContainer (Object _) = True
    isContainer _ = False

-- | Numbers convert to the same Double as GHC's correctly rounded reader.
prop_toDouble :: Property
prop_toDouble = forAll genDecimal $ \d -> toDouble d === read (BC.unpack (encode (Number d)))

-- Conformance --------------------------------------------------------------

-- | y_ files must parse, n_ files must not; i_ files may go either way, and
-- the outcomes are listed.
conformance :: IO Bool
conformance = do
  let dir = "tests" </> "JSONTestSuite" </> "test_parsing"
  files <- sort <$> listDirectory dir
  results <- forM files $ \f -> do
    s <- B.readFile (dir </> f)
    pure (f, isRight (decode s))
  let ys = [r | r@(f, _) <- results, "y_" `isPrefixOf` f]
      ns = [r | r@(f, _) <- results, "n_" `isPrefixOf` f]
      is = [r | r@(f, _) <- results, "i_" `isPrefixOf` f]
      badY = [f | (f, ok) <- ys, not ok]
      badN = [f | (f, ok) <- ns, ok]
  putStrLn $ "JSONTestSuite: accepted " ++ show (length ys - length badY) ++ " of " ++ show (length ys)
    ++ " y_ files, rejected " ++ show (length ns - length badN) ++ " of " ++ show (length ns) ++ " n_ files"
  putStrLn $ "  implementation-defined (i_): " ++ show (length [() | (_, True) <- is]) ++ " accepted, "
    ++ show (length [() | (_, False) <- is]) ++ " rejected"
  mapM_ (\(f, ok) -> putStrLn ("    " ++ (if ok then "accept " else "reject ") ++ f)) is
  mapM_ (putStrLn . ("  FAILED to accept " ++)) badY
  mapM_ (putStrLn . ("  FAILED to reject " ++)) badN
  pure (null badY && null badN)

-- Examples -----------------------------------------------------------------

examples :: [(String, Bool)]
examples =
  [ ("big integers are exact", ok "123456789012345678901234567890" (Number (decimal 123456789012345678901234567890 0)))
  , ("0.1 is exact", ok "0.1" (Number (decimal 1 (-1))))
  , ("1.50 equals 1.5", decode (bs "1.50") == decode (bs "1.5"))
  , ("huge exponents are kept", ok "1E400" (Number (decimal 1 400)))
  , ("negative zero is zero", ok "-0.0" (Number (decimal 0 0)))
  , ("rendering", encode (Number (decimal 15 (-1))) == bs "1.5" && encode (Number (decimal 1 (-7))) == bs "1e-7"
      && encode (Number (decimal 123 20)) == bs "1.23e22" && encode (Number (decimal (-5) (-1))) == bs "-0.5")
  , ("surrogate pair", ok "\"\\uD83D\\uDE00\"" (String (T.pack "\x1F600")))
  , ("raw UTF-8", decode (TE.encodeUtf8 (T.pack "\"\xe9\x4e2d\"")) == Right (String (T.pack "\xe9\x4e2d")))
  , ("invalid UTF-8 is rejected", isLeft (decode (B.pack [0x22, 0xff, 0x22])))
  , ("members keep their order", ok "{\"b\":1,\"a\":2}" (Object [(T.pack "b", num 1), (T.pack "a", num 2)]))
  , ("512 levels of nesting", isRight (decode (nested 512)))
  , ("513 levels are refused", isLeft (decode (nested 513)))
  , ("100000 open brackets fail fast", isLeft (decode (BC.replicate 100000 '[')))
  , err "{\"a\": 1,}" "line 1, column 9: expected a string key, found '}'"
  , err "[1, 2\n, 3x]" "line 2, column 4: expected ',' or ']', found 'x'"
  , err "\"abc" "line 1, column 5: expected '\"' to close the string, found end of input"
  , err "01" "line 1, column 1: expected a number without leading zeros, found '0'"
  , err "[tru]" "line 1, column 2: expected \"true\", found 't'"
  , err "{\"a\" 1}" "line 1, column 6: expected ':', found '1'"
  , err "[\"\\uDC00\"]" "line 1, column 3: expected a high surrogate before a low one, found '\\'"
  , err "\"a\tb\"" "line 1, column 3: expected a character (control characters must be escaped), found byte 0x09"
  , err "[1] x" "line 1, column 5: expected end of input, found 'x'"
  , err "" "line 1, column 1: expected a JSON value, found end of input"
  ]
  where
    bs = BC.pack
    num k = Number (decimal k 0)
    ok s v = decode (bs s) == Right v
    nested k = BC.replicate k '[' <> BC.replicate k ']'
    err s msg = ("error for " ++ show s, either renderError (const "parsed") (decode (bs s)) == msg)

-- Main ---------------------------------------------------------------------

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  let check name p = do
        r <- quickCheckWithResult stdArgs {maxSuccess = 1000, chatty = False} p
        putStrLn ((if isSuccess r then "ok   " else "FAIL ") ++ name ++ ": " ++ show (numTests r) ++ " cases")
        unless (isSuccess r) $ do
          putStrLn (output r)
          modifyIORef failures (+ 1)
  check "decode . encode = id" prop_roundTrip
  check "decode . encodePretty = id" prop_prettyRoundTrip
  check "encode . decode . encode = encode" prop_encodeIdempotent
  check "whitespace between tokens is ignored" prop_whitespace
  check "every proper prefix of an array or object fails" (mapSize (min 30) prop_prefixesFail)
  check "toDouble agrees with read" prop_toDouble
  conf <- conformance
  unless conf (modifyIORef failures (+ 1))
  mapM_ (\(name, ok) -> do
          putStrLn ((if ok then "ok   " else "FAIL ") ++ name)
          unless ok (modifyIORef failures (+ 1))) examples
  n <- readIORef failures
  when (n > 0) $ do
    putStrLn (show n ++ " failure(s)")
    exitFailure
  putStrLn "all passed"
