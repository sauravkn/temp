import           Data.Map.Strict as M (Map, fromList, difference, keys, intersection, toList, lookup, findWithDefault, empty)
import System.Environment

-- fp1 :: FilePath
-- fp1 =
--   "/Users/saurav/nfer/document-count-generator/app/src/docwiseFreqResultDiffer/sampleInp1"

-- fp2 :: FilePath
-- fp2 =
--   "/Users/saurav/nfer/document-count-generator/app/src/docwiseFreqResultDiffer/sampleInp2"

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =
  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
      where (w, s'') = break p s'

data DocumentData =
  MkDocumentData
    { unigramCount :: !Int
    , freqMap      :: !(Map Int Int)
    }
  deriving (Show, Eq)

type FileData = Map String DocumentData

-- map of documentId to documentData for each file
-- Single file contain multiple documents
splitFileIntoFreq :: String -> [((String, Int), Map Int Int)]
splitFileIntoFreq inp = zip (zip heads seconds) freqMap
  where
    val = lines inp
    heads = head <$> wrds
    seconds = readAsInt . head . tail <$> wrds
    wrds = wordsWhen (== ' ') <$> val
    freqs = splitInner . drop 2 <$> wrds
    freqMap = fromList <$> freqs

splitter :: Char -> Bool
splitter ' ' = True
splitter '\t' = True
splitter _ = False

splitFileIntoFreq' :: String -> Map String DocumentData
splitFileIntoFreq' inp = fromList (zip heads fdata)
  where
    val = lines inp
    wrds = wordsWhen splitter <$> val
    heads = head <$> wrds
    seconds = readAsInt . head . tail <$> wrds
    freqs = splitInner . drop 2 <$> wrds
    freqMap = fromList <$> freqs
    fdata = createFileData seconds freqMap

createFileData :: [Int] -> [Map Int Int] -> [DocumentData]
createFileData [] _          = []
createFileData _ []          = []
createFileData (c:cs) (m:ms) = MkDocumentData c m : createFileData cs ms

splitInner :: [String] -> [(Int, Int)]
splitInner inp = res1
  where
    res = wordsWhen (== ',') <$> inp
    res1 = makePairs res

makePairs :: [[String]] -> [(Int, Int)]
makePairs = map (\x -> (readAsInt . head $ x, readAsInt . (head . tail) $ x))

readAsInt :: String -> Int
readAsInt x = read x :: Int
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
    , documentsTokenFrequencyDifferent :: Int
    , documentsTokenFrequencySame      :: Int
    }
  deriving (Show)

documentDiff :: FileData  -> FileData -> Int
documentDiff m1 m2 = length $ keys $ difference m1 m2

commonDocCount :: FileData  -> FileData -> Int
commonDocCount m1 m2 = length $ keys $ intersection m1 m2

tokenwiseResult :: FileData -> FileData -> (Int,Int, Int, Int, Int)
tokenwiseResult m1 m2 = (a, b,c,d, e)
  where
    temp1 = toList m1
    temp2 = toList m2
    (a,b,c,d, e) = compareMap temp1 m2

compareMap :: [(String, DocumentData)] -> FileData -> (Int, Int, Int, Int, Int)
compareMap [] m2 = (0, 0, 0, 0, 0)
compareMap ((k, v):ms) m2 = if res == emptyDocumentData then  compareMap ms m2
                                        else add (v1, v2, v3, v4, v5) $ compareMap ms m2
                                          where
    res = M.findWithDefault emptyDocumentData k m2
    (v1, v2, v3, v4, v5) = compareDocumentData res v
    emptyDocumentData = MkDocumentData 0  M.empty
    add :: (Int,Int, Int, Int, Int) -> (Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int)
    add (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5) = (a1+b1, a2+b2, a3+b3, a4+b4, a5+b5)

correctingFactor = 14 -- 14 is constant difference due to algo difference

{-  unigramDiff
  , keyCountDiff1 keys present in 1 but not 2
  , keyCountDiff2 keys present in 2 but not in1
  , value Diff from keys common in both
-}
compareDocumentData :: DocumentData -> DocumentData -> (Int, Int, Int, Int, Int)
compareDocumentData d1 d2 = (uniGramCountDiff, v1, v2, v3, v4)
  where
    uniGramCountDiff = abs(unigramCount d1 - unigramCount d2) - correctingFactor
    (v1, v2, v3, v4) = compareFreqMap (freqMap d1) (freqMap d2)

compareFreqMap :: Map Int Int -> Map Int Int -> (Int, Int, Int, Int)
compareFreqMap m1 m2 = (keyCountDiff1, keyCountDiff2, commonKeyvalueDiff, commonKeyvalueSame)
  where
    keyCountDiff1 = length $ keys $ difference m1 m2
    keyCountDiff2 = length $ keys $ difference m2 m1
    commonKeyvalueDiff = compareIndividualKey (toList m1) m2
    commonKeyvalueSame = compareIndividualKey' (toList m1) m2

compareIndividualKey :: [(Int, Int)] -> Map Int Int -> Int
compareIndividualKey [] m2 = 0
compareIndividualKey ((k,v):ms) m2 = if value2 /= v then 1 + compareIndividualKey ms m2
                                                 else compareIndividualKey ms m2
  where
    emptyKeyVal = -1
    value2 = M.findWithDefault emptyKeyVal k m2

compareIndividualKey' :: [(Int, Int)] -> Map Int Int -> Int
compareIndividualKey' [] m2 = 0
compareIndividualKey' ((k,v):ms) m2 = if value2 == v then 1 + compareIndividualKey' ms m2
                                                 else compareIndividualKey' ms m2
  where
    emptyKeyVal = -1
    value2 = M.findWithDefault emptyKeyVal k m2

cmp :: FileData  -> FileData -> DiffStruct
cmp in1 in2 = MkDiffStruct (commonDocCount in1 in2) (documentDiff in1 in2) (documentDiff in2 in1) uniGram valueDiff valueSame
  where
    (uniGram, key12, key21, valueDiff, valueSame) = tokenwiseResult in1 in2

main :: IO ()
main = do
  args <- getArgs
  let fp1 = args !! 1
      fp2 = args !! 2
  inp1 <- readFile fp1
  inp2 <- readFile fp2
  print $ cmp (splitFileIntoFreq' inp1) (splitFileIntoFreq' inp2)

