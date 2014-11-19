{-# LANGUAGE BangPatterns #-}

module SLM.Gobble.GobbleDataTypes where

import SLM.DataTypes
import Data.Digest.Murmur32
import Data.Bits
import Data.Hashable
import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString.Lazy.Char8 as LC

data GobbleExample = GobbleExample {
    exampleIndices    :: V.Vector Int
  , exampleValues     :: Maybe (V.Vector Double)
  , exampleConstIndex :: Maybe Int
}



-- data GobbleX = GobbleX {
--    xIndex  :: Int
--  , xValue  :: Double
--} deriving (Show)

--makeGobbleX :: (String -> Int) -> Predictor -> GobbleX
--makeGobbleX hashFunc (Predictor name (FactorValue f))  = GobbleX (hashFunc (name ++ "_" ++ f)) 1.0
--makeGobbleX hashFunc (Predictor name (MeasureValue m)) = GobbleX (hashFunc name) m

--makeAltSpecificGobbleX :: (String -> Int) -> Predictor -> Alt -> GobbleX
--makeAltSpecificGobbleX hashFunc (Predictor varName (FactorValue f)) alt = GobbleX (hashFunc (alt ++ "_" ++ varName ++ "_" ++ f)) 1.0
--makeAltSpecificGobbleX hashFunc (Predictor varName (MeasureValue m)) alt = GobbleX (hashFunc (alt ++ "_" ++ varName)) m

--makeAltSpecificGobbleXs :: (String -> Int) -> [Predictor] -> Alt -> [GobbleX]
--makeAltSpecificGobbleXs hashFunc predictors alt = map (\predictor -> makeAltSpecificGobbleX hashFunc predictor alt) predictors

gobbleHash_ :: Int -> (LC.ByteString -> Int)
gobbleHash_ !maxValue = (\str -> (hash str) .&. maxValue)

gobbleHash :: Int -> (LC.ByteString -> Int)
gobbleHash bits = gobbleHash_ (2^bits - 1)

-- type GobbleXList = [GobbleX]

-- data GobbleObservation = GobbleObservation {
--     gobbleY   :: Double
--   , gobbleXs  :: [GobbleX]
-- } deriving (Show)

-- makeGobbleObservation :: Observation -> GobbleObservation
-- makeGobbleObservation (Observation y xlist) = GobbleObservation y (map makeGobbleX xlist)

-- data GobbleTrainingSet = GobbleTrainingSet { 
--   gobbleObservations :: [GobbleObservation]
-- }

-- makeGobbleTrainingSet :: TrainingSet -> GobbleTrainingSet
-- makeGobbleTrainingSet = GobbleTrainingSet . map makeGobbleObservation . observations
