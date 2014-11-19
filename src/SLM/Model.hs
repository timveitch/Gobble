module SLM.Model where

import SLM.DataTypes

class TrainedModel a where
  predictInstance :: a -> [Predictor] -> Double
  predict :: a -> [[Predictor]] -> [Double]
  predict model = map (predictInstance model)
