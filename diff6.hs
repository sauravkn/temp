{-# LANGUAGE OverloadedStrings     #-}

import           Data.Map.Strict as M (Map, fromList, difference, keys, intersection, toList, lookup, findWithDefault, empty)
import qualified    Data.IntMap.Strict as IntMap (IntMap, fromList, difference, keys, intersection, toList, lookup, findWithDefault, empty)
import System.Environment
import qualified Data.Text as T
import Data.Text.IO as TIO
import Data.Int


-- fp1 :: FilePath
-- fp1 =
--   "/Users/saurav/nfer/document-count-generator/app/src/docwiseFreqResultDiffer/sampleInp1"

-- fp2 :: FilePath
-- fp2 =
--   "/Users/saurav/nfer/document-count-generator/app/src/docwiseFreqResultDiffer/sampleInp2"

fp1 :: FilePath
fp1 =
  "/datastorage/sauravkumar/sorteda3_pruned"
  --"/datastorage/sauravkumar/sa3_100"

fp2 :: FilePath
fp2 =
  "/datastorage/sauravkumar/sorteda3_1"
  --"/datastorage/sauravkumar/soct_100"
-- wordsWhen :: (T.Text -> Bool) -> T.Text -> [T.Text]
-- wordsWhen p s =
--   case dropWhile p s of
--     "" -> []
--     s' -> w : wordsWhen p s''
--       where (w, s'') = break p s'

data DocumentData =
  MkDocumentData
    { unigramCount :: !Int
    , freqMap      :: !(IntMap.IntMap Int)
    }
  deriving (Show, Eq)

type FileData = Map T.Text DocumentData

splitter :: Char -> Bool
splitter ' ' = True
splitter '\t' = True
splitter _ = False

splitFileIntoFreq :: T.Text -> Map T.Text DocumentData
splitFileIntoFreq inp = fromList (zip heads fdata)
  where
    val = T.lines inp
    wrds = T.split splitter <$> val
    heads = head <$> wrds
    seconds = readAsInt . head . tail <$> wrds
    freqs = splitInner . drop 2 . init <$> wrds
    freqMap = IntMap.fromList <$> freqs
    fdata = createFileData seconds freqMap

createFileData :: [Int] -> [IntMap.IntMap Int] -> [DocumentData]
createFileData [] _          = []
createFileData _ []          = []
createFileData (c:cs) (m:ms) = MkDocumentData c m : createFileData cs ms

splitInner :: [T.Text] -> [(Int, Int)]
splitInner inp = res1
  where
    res = T.splitOn (T.singleton ',') <$> inp
    res1 = makePairs res

makePairs :: [[T.Text]] -> [(Int, Int)]
makePairs = map (\x -> (readAsInt . head $ x, readAsInt . (head . tail) $ x))

readAsInt :: T.Text -> Int
readAsInt x = read $ T.unpack x :: Int
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
    temp2 = toList m2
    (a,b,c,d, e) = compareMap temp1 m2

compareMap :: [(T.Text, DocumentData)] -> FileData -> (Int, Int, Int, Int64, Int64)
compareMap [] m2 = (0, 0, 0, 0, 0)
compareMap ((k, v):ms) m2 = if res == emptyDocumentData then  compareMap ms m2
                                        else add (v1, v2, v3, v4, v5) $ compareMap ms m2
                                          where
    res = M.findWithDefault emptyDocumentData k m2
    (v1, v2, v3, v4, v5) = compareDocumentData res v
    emptyDocumentData = MkDocumentData 0  IntMap.empty
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

compareFreqMap :: IntMap.IntMap Int -> IntMap.IntMap Int -> (Int, Int, Int64, Int64)
compareFreqMap m1 m2 = (keyCountDiff1, keyCountDiff2, commonKeyvalueDiff, commonKeyvalueSame)
  where
    keyCountDiff1 = length $ IntMap.keys $ IntMap.difference m1 m2
    keyCountDiff2 = length $ IntMap.keys $ IntMap.difference m2 m1
    commonKeyvalueDiff = compareIndividualKey (IntMap.toList m1) m2
    commonKeyvalueSame = compareIndividualKey' (IntMap.toList m1) m2

compareIndividualKey :: [(Int, Int)] -> IntMap.IntMap Int -> Int64
compareIndividualKey [] m2 = 0
compareIndividualKey ((k,v):ms) m2 = if value2 /= v then 1 + compareIndividualKey ms m2
                                                 else compareIndividualKey ms m2
  where
    emptyKeyVal = -1
    value2 = IntMap.findWithDefault emptyKeyVal k m2

compareIndividualKey' :: [(Int, Int)] -> IntMap.IntMap Int -> Int64
compareIndividualKey' [] m2 = 0
compareIndividualKey' ((k,v):ms) m2 = if value2 == v then 1 + compareIndividualKey' ms m2
                                                 else compareIndividualKey' ms m2
  where
    emptyKeyVal = -1
    value2 = IntMap.findWithDefault emptyKeyVal k m2

cmp :: FileData  -> FileData -> DiffStruct
cmp in1 in2 = MkDiffStruct (commonDocCount in1 in2) (documentDiff in1 in2) (documentDiff in2 in1) uniGram valueDiff valueSame
  where
    (uniGram, key12, key21, valueDiff, valueSame) = tokenwiseResult in1 in2

main :: IO ()
main = do
  -- args <- getArgs
  -- let fp1 = args !! 1
  --     fp2 = args !! 2
  inp1 <- TIO.readFile fp1
  inp2 <- TIO.readFile fp2
  print $ cmp (splitFileIntoFreq inp1) (splitFileIntoFreq inp2)

