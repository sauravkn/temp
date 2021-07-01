{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}

import qualified    Data.IntMap as IM (IntMap, fromList, difference, keys, intersection, toList, lookup, findWithDefault, empty, size)
import System.Environment
import qualified Data.Text as L
import Data.Text.IO as LTIO
import Data.Int


fp1 :: FilePath
fp1 =
  -- "/datastorage/sauravkumar/sorteda3_pruned"
  "/datastorage/sauravkumar/sa3_10000"

fp2 :: FilePath
fp2 =
  -- "/datastorage/sauravkumar/sortedOrigClinT"
  "/datastorage/sauravkumar/sa3_1_10000"

splitter :: Char -> Bool
splitter ' ' = True
splitter '\t' = True
splitter _ = False

splitInner :: [L.Text] -> [(Int, Int)]
splitInner !inp = res1
  where
    !res = L.splitOn (L.singleton ',') <$> inp
    !res1 = makePairs res

makePairs :: [[L.Text]] -> [(Int, Int)]
makePairs = map (\x -> (readAsInt . head $! x, readAsInt . (head . tail) $! x))

readAsInt :: L.Text -> Int
readAsInt !x = read $ L.unpack x :: Int

{-
   add' will contain
   - document present only in first
   - document present only in second
   * same document
     - token present only in first
     - token present only in second
     - token present in both but different frequency
     - token present in both and same frequency
-}
data DiffStruct =
  MkDiffStruct
    { documentsPresentOnlyInFirst      :: !Int
    , documentsPresentOnlyInSecond     :: !Int
    , tokensPresentOnlyInFirst         :: !Int
    , tokensPresentOnlyInSecond        :: !Int
    , tokenFrequencySame               :: !Int64
    , tokenFrequencyDifferent          :: !Int64
    }
  deriving (Show)


readBothTogether :: L.Text -> L.Text -> DiffStruct
readBothTogether t1 t2 = MkDiffStruct a b c d e f
  where
    !wrds1 = L.split splitter <$> L.lines t1
    !wrds2 = L.split splitter <$> L.lines t2
    (a,b,c,d,e,f) = compare' wrds1 wrds2 (0,0,0,0,0,0)

add' :: (Int,Int, Int, Int, Int64, Int64) -> (Int, Int, Int, Int, Int64, Int64) -> (Int, Int, Int, Int, Int64, Int64)
add' (a1, a2, a3, a4, a5, a6) (b1, b2, b3, b4, b5, b6) = (a1+b1, a2+b2, a3+b3, a4+b4, a5+b5, a6+b6)

compare' :: [[L.Text]] -> [[L.Text]] -> (Int, Int, Int, Int, Int64, Int64) -> (Int, Int, Int, Int, Int64, Int64)
compare' _ [] !acc  = acc
compare' [] _ !acc = acc
compare' inp1@(x:xs) inp2@(y:ys) !acc
  | head1 > head2 = compare' xs inp2 $! add' (1,0,0,0,0,0) acc
  | head1 < head2 = compare' inp1 ys $! add' (0,1,0,0,0,0) acc
  | otherwise =  compare' xs ys (add' (0, 0, tokensPresentOnlyInFirst, tokensPresentOnlyInSecond, sameVal, diffVal) acc)
  where
    !head1 = head x
    seconds1 = readAsInt . head . tail $! x
    freqs1 = IM.fromList . splitInner . drop 2 . init $! x

    !head2 = head y
    seconds2 = readAsInt . head . tail $! y
    freqs2 = IM.fromList . splitInner . drop 2 . init $! y

    tokensPresentOnlyInFirst = IM.size $! IM.difference freqs1 freqs2
    tokensPresentOnlyInSecond = IM.size $! IM.difference freqs2 freqs1
    commonKeys = IM.intersection freqs1 freqs2
    (sameVal, diffVal) = compareCommonKeysInTwoMaps (IM.keys commonKeys) freqs1 freqs2 (0, 0)

compareCommonKeysInTwoMaps :: [Int] -> IM.IntMap Int -> IM.IntMap Int -> (Int64, Int64) -> (Int64, Int64)
compareCommonKeysInTwoMaps [] _ _ !acc = acc
compareCommonKeysInTwoMaps (x:xs) m1 m2 !acc
  | val1 == val2 = compareCommonKeysInTwoMaps xs m1 m2 $! add2 acc (1,0)
  | otherwise = compareCommonKeysInTwoMaps xs m1 m2 $! add2 acc (0,1)
  where
    val1 = IM.findWithDefault (-1) x m1
    val2 = IM.findWithDefault (-1) x m2

add2 :: (Int64, Int64) -> (Int64, Int64) -> (Int64, Int64)
add2 (a1, a2) (b1, b2) = (a1+b1, a2+b2)

main :: IO ()
main = do
  -- args <- getArgs
  -- let fp1 = args !! 1
  --     fp2 = args !! 2
  inp1 <- LTIO.readFile fp1
  inp2 <- LTIO.readFile fp2

  print $ readBothTogether inp1 inp2

