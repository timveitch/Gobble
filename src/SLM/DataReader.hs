
module SLM.DataReader where

import SLM.DataTypes
import Data.List.Split
import qualified Data.Vector as V

--readDelimitedPredictors :: String -> [String] -> [[Predictor]]
--readDelimitedPredictors delimiter (header:rows) = map makePredictorList rows
--  where predictorDefinitions = (map makePredictorDefinition) . (splitOn delimiter) $ header
--        makePredictorList = zipWith makePredictor predictorDefinitions . (splitOn delimiter)

--readPredictorsFromCsv = readDelimitedPredictors “,”