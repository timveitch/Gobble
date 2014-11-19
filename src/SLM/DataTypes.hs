
module SLM.DataTypes where

import Data.List.Split        
import qualified Data.ByteString.Lazy.Char8 as LC

type PredictorName = LC.ByteString
data PredictorType = FactorType
                   | MeasureType
                   | InvalidType
  deriving (Show)

data PredictorDefinition = PredictorDefinition !PredictorName !PredictorType
  deriving (Show)

isFactorVariable :: PredictorDefinition -> Bool
isFactorVariable (PredictorDefinition _ FactorType) = True
isFactorVariable _ = False
 
data PredictorValue = FactorValue  String
                    | MeasureValue Double
                    | NA
  deriving (Show)

data Predictor = Predictor PredictorName PredictorValue
  deriving (Show)

type Alt = String
data ChoiceSetWithChosenAlt = ChoiceSetWithChosenAlt {
    chosen  :: Alt
  , allAlts :: [Alt]
} deriving Show

makePredictorDefinition :: LC.ByteString -> PredictorDefinition
makePredictorDefinition = processTokenisedPredictorDefinition . (splitOn ":") . LC.unpack

processTokenisedPredictorDefinition :: [String] -> PredictorDefinition
processTokenisedPredictorDefinition (name:[]) = PredictorDefinition (LC.pack name) FactorType
processTokenisedPredictorDefinition (name:"F":[]) = PredictorDefinition (LC.pack name) FactorType
processTokenisedPredictorDefinition (name:"N":[]) = PredictorDefinition (LC.pack name) MeasureType
processTokenisedPredictorDefinition (name:_) = PredictorDefinition (LC.pack name) FactorType

makePredictor :: PredictorDefinition -> String -> Predictor
makePredictor (PredictorDefinition name FactorType) = Predictor name . FactorValue
makePredictor (PredictorDefinition name MeasureType) = Predictor name . MeasureValue . read 

data Observation = Observation {
    y  :: Double
  , xs :: [Predictor]
}

data TrainingSet = TrainingSet {
  observations :: [Observation]
}


