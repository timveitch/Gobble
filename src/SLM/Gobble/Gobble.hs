
module SLM.Gobble.Gobble where

import qualified Data.IntMap as IM
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import SLM.DataTypes
import SLM.Gobble.GobbleDataTypes
import SLM.Gobble.GobbleArgs
import Control.Applicative
import qualified Data.Foldable as F

-- PUBLIC INTERFACE
gobble :: GobbleArgs -> Bool -> Int -> [[Predictor]] -> [Double] -> [Double] -> GobbleModel
gobble args parallel seed xs ys ws = gobbleGobble args parallel seed (map (map (makeGobbleX (bits args))) xs) ys ws

-- INTERNAL INTERFACE
gobbleGobble :: GobbleArgs -> Bool -> Int -> [[GobbleX]] -> [Double] -> [Double] -> GobbleModel
gobbleGobble args parallel seed xs ys ws = F.foldl' f (newGobbleModel (bits args)) xys
  where xys = zipWith (\x y -> (x,y)) xs ys
        gradFunc = gobbleGradientLogistic
        predFunc = gobbleGobblePredictLogistic
        f inputModel (x,y) = gobbleUpdateOnObs gradFunc predFunc x y inputModel

pair :: a -> b -> (a,b)
pair x y = (x,y)

-- GOBBLE MODEL UPDATE
gobbleUpdateOnObs :: GradientFunc -> PredictionFunc -> [GobbleX] -> Double -> GobbleModel -> GobbleModel
gobbleUpdateOnObs gradientFunc predictionFunc xs actual inputModel = F.foldl' f inputModel xs
  where prediction = predictionFunc inputModel xs
        f model x = gobbleUpdateOnSinglePredictor gradientFunc x actual prediction model

alpha = 0.1
power = 0.5

gobbleUpdateOnSinglePredictor :: GradientFunc -> GobbleX -> Double -> Double -> GobbleModel -> GobbleModel
gobbleUpdateOnSinglePredictor gradientFunc (GobbleX index x) actual prediction (GobbleModel weightGrads) = GobbleModel (updateWeightGrads weightGrads index newWeightGrad) -- updateGobbleModel inputModel index weightDelta gradientDelta
  where prevWeightGrad = weightGrads V.! index --IM.findWithDefault (WeightGrad 0.0 0.0) index (weightGrads inputModel)
        prevSumGradients = 1.0 + (grad prevWeightGrad)
        gradient = gradientFunc x actual prediction
        weightDelta = -gradient * alpha / (prevSumGradients ** power)
        gradientDelta = abs gradient
        newWeightGrad = incWeightGrad' prevWeightGrad weightDelta gradientDelta

-- GOBBLE MODEL
newGobbleModel :: Int -> GobbleModel
newGobbleModel bits = GobbleModel (V.replicate (2^bits) (WeightGrad 0 0)) 

data WeightGrad = WeightGrad {
    weight :: Double
  , grad   :: Double
} deriving (Show,Read)

incWeightGrad' :: WeightGrad -> Double -> Double -> WeightGrad
incWeightGrad' (WeightGrad w1 g1) w2 g2 = WeightGrad (w1+w2) (g1+g2)

incWeightGrad :: WeightGrad -> WeightGrad -> WeightGrad
incWeightGrad (WeightGrad w1 g1) (WeightGrad w2 g2) = WeightGrad (w1 + w2) (g1 + g2)

data GobbleModel = GobbleModel {
    weightGrads :: !(V.Vector WeightGrad)
} deriving (Show,Read)

getWeightGrad :: GobbleModel -> Int -> WeightGrad
getWeightGrad (GobbleModel wgs) index = wgs V.! index

getWeight :: GobbleModel -> Int -> Double
getWeight (GobbleModel wgs) index = weight (wgs V.! index) -- (IM.findWithDefault (WeightGrad 0.0 0.0) index (weightGrads model))

-- updateGobbleModel :: GobbleModel -> Int -> Double -> Double -> GobbleModel
-- updateGobbleModel (GobbleModel weightGrads) index weightDelta gradDelta = GobbleModel (updateWeightGrads weightGrads index WeightGrad)

-- $! (IM.insertWith incWeightGrad index (WeightGrad weightDelta gradDelta) (weightGrads inputModel))

updateWeightGrads :: V.Vector WeightGrad -> Int -> WeightGrad -> V.Vector WeightGrad
updateWeightGrads v index w = V.modify (\v -> VM.write v index w) v

-- PREDICTION
type PredictionFunc   = GobbleModel -> [GobbleX] -> Double
gobblePredictLogistic :: Int -> GobbleModel -> [Predictor] -> Double
gobblePredictLogistic bits model predictors = gobbleGobblePredictLogistic model (map (makeGobbleX bits) predictors)

logistic :: Double -> Double
logistic z = 1.0 / (1.0 + exp(-z))

inverseLogistic :: Double -> Double
inverseLogistic probability = -(log (1.0 / probability - 1))

sum' :: [Double] -> Double
sum' xs = F.foldl' (+) 0 xs

gobbleGobblePredictLogistic :: GobbleModel -> [GobbleX] -> Double
gobbleGobblePredictLogistic model xs = logistic z
  where weights = map (\x -> getWeight model (xIndex x)) xs
        xValues = map xValue xs
        z = sum' ( (*) <$> weights <*> xValues)

-- GRADIENTS
type GradientFunc = Double -> Double -> Double -> Double

gobbleGradientLogistic :: Double -> Double -> Double -> Double
gobbleGradientLogistic x actual prediction = (-1) * x * (actual * (actual - prediction) + (1 - actual) * (actual - (1 - prediction)))

