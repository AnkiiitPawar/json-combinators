-- | Decoding and encoding throughput on a generated 20 MB document: an array
-- of records with numbers, strings (some escaped, some non-ASCII), booleans
-- and nested arrays. Writes the document to bench-doc.json so that
-- scripts/bench_python.py can time Python's json module on the same bytes.
module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (forM)
import qualified Data.ByteString as B
import Data.List (foldl')
import qualified Data.Text as T
import GHC.Clock (getMonotonicTime)
import JsonCombinators
import Text.Printf (printf)

record :: Integer -> Value
record i =
  Object
    [ (T.pack "id", Number (decimal i 0))
    , (T.pack "name", String (T.pack ("user" ++ show i)))
    , (T.pack "score", Number (decimal ((i * 7919) `mod` 100003) (-2)))
    , (T.pack "ratio", Number (decimal ((i * 104729) `mod` 999983) (-9)))
    , (T.pack "active", Bool (even i))
    , (T.pack "tags", Array [String (T.pack "alpha"), String (T.pack "beta"), Null])
    , (T.pack "note", String (T.pack "a \"quoted\" word, a tab\t, caf\233 and \20013\25991"))
    , (T.pack "point", Object [(T.pack "x", Number (decimal (i `mod` 1000) (-1))), (T.pack "y", Number (decimal (negate i) 0))])
    ]

-- | Touches every node, so that timing includes building the whole tree.
size :: Value -> Int
size (Array xs) = foldl' (\n v -> n + size v) 1 xs
size (Object kvs) = foldl' (\n (k, v) -> n + T.length k + size v) 1 kvs
size (String t) = T.length t
size _ = 1

best :: Int -> (Int -> IO a) -> IO Double
best k act = minimum <$> forM [1 .. k] (\i -> do
  t0 <- getMonotonicTime
  _ <- act i
  t1 <- getMonotonicTime
  pure (t1 - t0))

-- The NOINLINE wrappers, and inputs that depend on the iteration number, stop
-- GHC from computing a result once and sharing it between timed iterations.
decodeSize :: B.ByteString -> IO Int
decodeSize s = case decode s of
  Left e -> error (renderError e)
  Right v -> evaluate (size v)
{-# NOINLINE decodeSize #-}

encodedLength :: (Value -> B.ByteString) -> Value -> IO Int
encodedLength f v = evaluate (B.length (f v))
{-# NOINLINE encodedLength #-}

main :: IO ()
main = do
  let doc = Array (map record [1 .. 110000])
      text = encode doc
  B.writeFile "bench-doc.json" text
  let mb = fromIntegral (B.length text) / 1e6 :: Double
  printf "document: %.1f MB, %d nodes\n" mb (size doc)
  tDec <- best 5 $ \i -> decodeSize (if i < 0 then B.empty else text)
  tEnc <- best 5 $ \i -> encodedLength encode (if i < 0 then Null else doc)
  tPretty <- best 5 $ \i -> encodedLength encodePretty (if i < 0 then Null else doc)
  printf "decode        %7.1f MB/s  (%.3f s)\n" (mb / tDec) tDec
  printf "encode        %7.1f MB/s  (%.3f s)\n" (mb / tEnc) tEnc
  printf "encodePretty  %7.1f MB/s of compact input (%.3f s)\n" (mb / tPretty) tPretty
