{-# LANGUAGE BangPatterns #-}

module SLM.Gobble.GobbleDataReader where

-- import SLM.Utils
import SLM.DataTypes
import Data.List.Split
import SLM.Gobble.GobbleDataTypes
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as VM

import qualified Data.ByteString.Lazy.Char8 as LC
import Data.ByteString.Lex.Lazy.Double
import System.IO
import Data.Word

readDelimitedPredictors :: Char -> (LC.ByteString -> Int) -> Bool -> FilePath -> IO [GobbleExample]
readDelimitedPredictors delimiter hashFunc includeConst filename = do
  fileContents <- LC.readFile filename
  return (processDelimitedPredictors delimiter hashFunc includeConst (LC.lines fileContents))

processDelimitedPredictors :: Char -> (LC.ByteString -> Int) -> Bool -> [LC.ByteString] -> [GobbleExample]
processDelimitedPredictors delimiter !hashFunc includeConst (header:rows) =  map (processRow hashFunc includeConst headerDef) splitRows
  where headerDef = processHeader delimiter header
        splitRows = map (LC.split delimiter) rows

data HeaderDef = HeaderDef ![PredictorDefinition] !Bool

processHeader :: Char -> LC.ByteString -> HeaderDef
processHeader delimiter header = HeaderDef predictorDefs allFactors
  where predictorDefs = (map makePredictorDefinition) . (LC.split delimiter) $ header
        allFactors    = all isFactorVariable predictorDefs

--processRows :: (String -> Int) -> Bool -> HeaderDef -> [[String]] -> [GobbleExample]
--processRows hashFunc includeConst !(HeaderDef predictorDefs allFactors) rows = map (processRow hashFunc includeConst allFactors predictorDefs) rows

processRow :: (LC.ByteString -> Int) -> Bool -> HeaderDef -> [LC.ByteString] -> GobbleExample
processRow hashFunc includeConst !(HeaderDef predictorDefs allFactors) rowItems = defined `seq` GobbleExample (indices defined) (xs defined) constIndex
  where zipped   = zipWith (\a b -> (a,b)) predictorDefs rowItems
        defined  = filter (\(_,val) -> val /= (LC.pack "NA")) zipped
        indices !def = VU.fromList $ map (uncurry $ createIndex hashFunc) def
        xs      !def = if allFactors then Nothing else Just . VU.fromList $ map (uncurry createXValue) def
        constIndex   = if not includeConst then Nothing else Just $ createIndex hashFunc (PredictorDefinition (LC.pack "const") FactorType) (LC.pack "1")

createIndex :: (LC.ByteString -> Int) -> PredictorDefinition -> LC.ByteString -> Int
createIndex hashFunc !(PredictorDefinition name FactorType) rowItem = hashFunc (LC.concat [name,LC.pack "_",rowItem])
createIndex hashFunc !(PredictorDefinition name MeasureType) _ = hashFunc name

createXValue :: PredictorDefinition -> LC.ByteString -> Double
createXValue (PredictorDefinition _ FactorType)    _ = 1.0
createXValue (PredictorDefinition _ MeasureType) val = readDoubleBs val

readPredictorsFromCsv = readDelimitedPredictors ','

readWeightsFile :: FilePath -> Int -> IO (V.Vector (VU.Vector Double))
readWeightsFile filename bits = do
  withFile filename ReadMode (\handle -> do 
    contents <- LC.hGetContents handle
    let lines = LC.lines contents
    let dim = readIntBs (head lines)
    multiWtsVec <- V.replicateM dim (VM.replicate (vecSize bits) 0) 
    let splitLines = map (LC.split ',') (tail lines)
    mapM_ (\(a:b:c:_) -> do
      let varIndex = readIntBs a
      let weightIndex = readIntBs b
      let weight = readDoubleBs c
      VM.write (multiWtsVec V.! varIndex) weightIndex weight) splitLines
    V.mapM (VU.unsafeFreeze) multiWtsVec)


processYsWithoutHeader :: [[LC.ByteString]] -> [V.Vector Double]
processYsWithoutHeader = map processMultiYLine

processYsWithHeader :: [[LC.ByteString]] -> [V.Vector Double]
processYsWithHeader contents = processYsWithoutHeader (tail contents)

readYsWithHeader :: FilePath -> IO ([V.Vector Double])
readYsWithHeader filename = do
  contents <- LC.readFile filename
  let splitContents = map (LC.split ',') (LC.lines contents)
  return (processYsWithHeader splitContents)


processMultiYLine :: [LC.ByteString] -> V.Vector Double
processMultiYLine items = V.map readDoubleBs (V.fromList items)


readMultiClassYsWithoutHeader :: [String] -> [ChoiceSetWithChosenAlt]
readMultiClassYsWithoutHeader lines = map processMultiClassYsLine lines

processMultiClassYsLine :: String -> ChoiceSetWithChosenAlt
processMultiClassYsLine line = ChoiceSetWithChosenAlt chosen avails
  where splitLine = splitOn "|" line
        chosen    = head splitLine
        avails    = splitOn "," (last splitLine)

readMultiClassYsWithHeader :: [String] -> [ChoiceSetWithChosenAlt]
readMultiClassYsWithHeader lines = readMultiClassYsWithoutHeader (tail lines)

readAvailableAltsFromFile :: [String] -> [[Alt]]
readAvailableAltsFromFile lines = map readAvailableAltsLine (tail lines)

readAvailableAltsLine :: String -> [Alt]
readAvailableAltsLine = splitOn ","


vecSize :: Int -> Int
vecSize bits = 2^bits

readIntBs :: LC.ByteString -> Int
readIntBs bs = case LC.readInt bs of Nothing -> error "Not an integer"
                                     Just (x, _) -> x

readDoubleBs :: LC.ByteString -> Double
readDoubleBs bs = case readDouble bs of Nothing -> error "Not a double"
                                        Just (x,_) -> x

