{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE BangPatterns     #-}

import           Data.Map.Strict as M (Map, fromList, difference, keys, intersection, toList, lookup, findWithDefault, empty)
import qualified    Data.IntMap.Strict as IM (IntMap, fromList, difference, keys, intersection, toList, lookup, findWithDefault, empty, size)
import System.Environment

import qualified Data.Text.Lazy as L
import Data.Text.Lazy.IO as LTIO
import Data.Int


fp1 :: FilePath
fp1 =
  --"/datastorage/sauravkumar/sorteda3_pruned"
  "/datastorage/sauravkumar/sa3_10000"

fp2 :: FilePath
fp2 =
   --"/datastorage/sauravkumar/sorteda3_1"
  "/datastorage/sauravkumar/sa3_1_10000"

data DocumentData =
  MkDocumentData
    { unigramCount :: !Int
    , freqMap      :: !(IM.IntMap Int)
    }
  deriving (Show, Eq)

type FileData = Map L.Text DocumentData

splitter :: Char -> Bool
splitter ' ' = True
splitter '\t' = True
splitter _ = False


getSecond :: [T.Text] -> T.Text
getSecond (a:b:_) = b

createFileData :: [Int] -> [IM.IntMap Int] -> [DocumentData]
createFileData [] _          = []
createFileData _ []          = []
createFileData (c:cs) (m:ms) = MkDocumentData c m : createFileData cs ms

splitInner :: [L.Text] -> [(Int, Int)]
splitInner inp = res1
  where
    res = L.splitOn (L.singleton ',') <$> inp
    res1 = makePairs res

makePairs :: [[L.Text]] -> [(Int, Int)]
makePairs = map (\x -> (readAsInt . head $ x, readAsInt . (head . tail) $ x))

readAsInt :: L.Text -> Int
readAsInt x = read $! L.unpack x :: Int
 {-
    Comparing result of two files need to take care of:
    + docuemtns present in result of only 1 file
    + common documents (present in both file's result)
      - missing token in one of file's result.
      - common token, but frequency different
      - happy scenario, frequency match too.
 -}

data DiffStruct =
  MkDiffStruct
    { documentsPresentInBoth           :: Int
    , documentsPresentOnlyInFirst      :: Int
    , documentsPresentOnlyInSecond     :: Int
    , documentsTokenCountDifferent     :: Int
    , documentsTokenFrequencyDifferent :: Int64
    , documentsTokenFrequencySame      :: Int64
    }
  deriving (Show)

documentDiff :: FileData  -> FileData -> Int
documentDiff m1 m2 = length $ keys $ difference m1 m2

commonDocCount :: FileData  -> FileData -> Int
commonDocCount m1 m2 = length $ keys $ intersection m1 m2

tokenwiseResult :: FileData -> FileData -> (Int,Int, Int, Int64, Int64)
tokenwiseResult m1 m2 = (a, b,c,d, e)
  where
    temp1 = toList m1
    -- temp2 = toList m2
    (a,b,c,d, e) = compareMap temp1 m2

compareMap :: [(L.Text, DocumentData)] -> FileData -> (Int, Int, Int, Int64, Int64)
compareMap [] m2 = (0, 0, 0, 0, 0)
compareMap ((k, v):ms) m2 = if res == emptyDocumentData then  compareMap ms m2
                                        else add (v1, v2, v3, v4, v5) $ compareMap ms m2
                                          where
    res = M.findWithDefault emptyDocumentData k m2
    (v1, v2, v3, v4, v5) = compareDocumentData res v
    emptyDocumentData = MkDocumentData 0  IM.empty

add :: (Int,Int, Int, Int64, Int64) -> (Int, Int, Int, Int64, Int64) -> (Int, Int, Int, Int64, Int64)
add (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5) = (a1+b1, a2+b2, a3+b3, a4+b4, a5+b5)

correctingFactor = 14 -- 14 is constant difference due to algo difference

{-  unigramDiff
  , keyCountDiff1 keys present in 1 but not 2
  , keyCountDiff2 keys present in 2 but not in1
  , value Diff from keys common in both
-}
compareDocumentData :: DocumentData -> DocumentData -> (Int, Int, Int, Int64, Int64)
compareDocumentData d1 d2 = (uniGramCountDiff, v1, v2, v3, v4)
  where
    uniGramCountDiff = abs(unigramCount d1 - unigramCount d2) - correctingFactor
    (v1, v2, v3, v4) = compareFreqMap (freqMap d1) (freqMap d2)

compareFreqMap :: IM.IntMap Int -> IM.IntMap Int -> (Int, Int, Int64, Int64)
compareFreqMap m1 m2 = (keyCountDiff1, keyCountDiff2, commonKeyvalueDiff, commonKeyvalueSame)
  where
    keyCountDiff1 = length $ IM.keys $ IM.difference m1 m2
    keyCountDiff2 = length $ IM.keys $ IM.difference m2 m1
    commonKeyvalueDiff = compareIndividualKey (IM.toList m1) m2
    commonKeyvalueSame = compareIndividualKey' (IM.toList m1) m2

compareIndividualKey :: [(Int, Int)] -> IM.IntMap Int -> Int64
compareIndividualKey [] m2 = 0
compareIndividualKey ((k,v):ms) m2 = if value2 /= v then 1 + compareIndividualKey ms m2
                                                 else compareIndividualKey ms m2
  where
    emptyKeyVal = -1
    value2 = IM.findWithDefault emptyKeyVal k m2

compareIndividualKey' :: [(Int, Int)] -> IM.IntMap Int -> Int64
compareIndividualKey' [] m2 = 0
compareIndividualKey' ((k,v):ms) m2 = if value2 == v then 1 + compareIndividualKey' ms m2
                                                 else compareIndividualKey' ms m2
  where
    emptyKeyVal = -1
    value2 = IM.findWithDefault emptyKeyVal k m2

cmp :: FileData  -> FileData -> DiffStruct
cmp in1 in2 = MkDiffStruct (commonDocCount in1 in2) (documentDiff in1 in2) (documentDiff in2 in1) uniGram valueDiff valueSame
  where
    (uniGram, key12, key21, valueDiff, valueSame) = tokenwiseResult in1 in2

splitFileIntoFreq :: L.Text -> Map L.Text DocumentData
splitFileIntoFreq inp = fromList (zip heads fdata)
  where
    val = L.lines inp
    wrds = L.split splitter <$> val
    heads = head <$> wrds
    seconds = readAsInt . head . tail <$> wrds
    freqs = splitInner . drop 2 . init <$> wrds
    freqMap = IM.fromList <$> freqs
    fdata = createFileData seconds freqMap

readBothTogether :: L.Text -> L.Text -> DiffStruct
readBothTogether t1 t2 = MkDiffStruct a b c d e f
  where
    wrds1 = L.split splitter <$> L.lines t1
    wrds2 = L.split splitter <$> L.lines t2
    (a,b,c,d,e,f) = compare' wrds1 wrds2

add' :: (Int,Int, Int, Int, Int64, Int64) -> (Int, Int, Int, Int, Int64, Int64) -> (Int, Int, Int, Int, Int64, Int64)
add' (a1, a2, a3, a4, a5, a6) (b1, b2, b3, b4, b5, b6) = (a1+b1, a2+b2, a3+b3, a4+b4, a5+b5, a6+b6)
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

compare' :: [[L.Text]] -> [[L.Text]] -> (Int, Int, Int, Int, Int64, Int64)
compare' _ []  = (0,0,0,0,0, 0)
compare' [] _ = (0,0,0,0,0, 0)
compare' inp1@(x:xs) inp2@(y:ys)
  | head1 > head2 = add' (1,0,0,0,0,0) $ compare' xs inp2
  | head1 < head2 = add' (0,1,0,0,0,0) $ compare' inp1 ys
  | otherwise = add' (0, 0, tokensPresentOnlyInFirst, tokensPresentOnlyInSecond, sameVal, diffVal) $ compare' xs ys
  where
    head1 = head x
    seconds1 = readAsInt . head . tail $ x
    freqs1 = IM.fromList . splitInner . drop 2 . init $ x

    head2 = head y
    seconds2 = readAsInt . head . tail $ y
    freqs2 = IM.fromList . splitInner . drop 2 . init $ y

    tokensPresentOnlyInFirst = IM.size $ IM.difference freqs1 freqs2
    tokensPresentOnlyInSecond = IM.size $ IM.difference freqs2 freqs1
    commonKeys = IM.intersection freqs1 freqs2
    (sameVal, diffVal) = compareCommonKeysInTwoMaps (IM.keys commonKeys) freqs1 freqs2

compareCommonKeysInTwoMaps :: [Int] -> IM.IntMap Int -> IM.IntMap Int -> (Int64, Int64)
compareCommonKeysInTwoMaps [] _ _ = (0, 0)
compareCommonKeysInTwoMaps (x:xs) m1 m2
  | val1 == val2 = add2 (1, 0) $ compareCommonKeysInTwoMaps xs m1 m2
  | otherwise = add2 (0, 1) $ compareCommonKeysInTwoMaps xs m1 m2
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

