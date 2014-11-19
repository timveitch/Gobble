{-# LANGUAGE BangPatterns #-}

module SLM.Gobble.GobbleMultiClassM where

import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Vector.Unboxed as V
import qualified Data.Foldable as F
import Control.Monad.Primitive

import SLM.Gobble.GobbleM
import SLM.Gobble.GobbleDataTypes
import SLM.Gobble.GobbleArgs
import SLM.DataTypes


-- PUBLIC INTERFACE

gobbleMultiClass :: GobbleArgs -> [[Predictor]] -> [Alt] -> [[Alt]] -> [Double] -> IO (V.Vector Double)
gobbleMultiClass args xs chosenAlts availableAlts ws = do
    gobbleGobbleMultiClass args availGobbleXs chosenAlts availableAlts ws
  where hashFunc      = murmurHash (bits args)
        availGobbleXs = zipWith (expandedGobbleXs hashFunc) xs availableAlts -- type [[[GobbleX]]], list by obs x alt x var

expandedGobbleXs :: (String -> Int) -> [Predictor] -> [Alt] -> [[GobbleX]]
expandedGobbleXs hashFunc predictors alts = map (\alt -> makeAltSpecificGobbleXs hashFunc predictors alt) alts

gobbleGobbleMultiClass :: GobbleArgs -> [[[GobbleX]]] -> [Alt] -> [[Alt]] -> [Double] -> IO (V.Vector Double)
gobbleGobbleMultiClass args xs chosenAlts availableAlts ws = do
    wtsVec <- VM.replicate size 0
    gsVec  <- VM.replicate size 1

    mapM_ (\(choiceSet,chosen) -> processMultiClassObs wtsVec gsVec gradFunc predFunc choiceSet chosen) observations
    V.unsafeFreeze wtsVec
  where size = vecSize (bits args)
        gradFunc = gobbleGradientLogistic
        predFunc = predictMultiClassLogit
        choiceSets = zipWith MultiClassChoiceSet xs availableAlts
        observations = zipWith pair choiceSets chosenAlts

type MVecD = VM.MVector RealWorld Double

data MultiClassChoiceSet = MultiClassChoiceSet {
    choiceSetXs :: [[GobbleX]]
  , choiceSetAlts :: [Alt]
}

data AltData = AltData {
    xs         :: [GobbleX]
  , prediction :: Double
  , actual     :: Double
}

processMultiClassObs :: MVecD -> MVecD -> GradientFunc -> MultiClassPredictionFunc -> MultiClassChoiceSet -> Alt -> IO ()
processMultiClassObs wtsVec gsVec gradFunc predFunc (MultiClassChoiceSet !availXs !availAlts) !chosenAlt = do
    ws <- mapM (mapM (VM.read wtsVec)) indicies
    let probabilities = predFunc ws xs
    let altDatas      = (zipWith3 AltData availXs probabilities actuals)
    mapM_ f altDatas
  where indicies  = map (map xIndex) availXs
        xs        = map (map xValue) availXs
        actuals   = map (\av -> if av == chosenAlt then 1.0 else 0.0) availAlts
        f (AltData xs pred actual) = processObs_ wtsVec gsVec gradFunc pred (xs,actual)

type MultiClassPredictionFunc = [[Double]] -> [[Double]] -> [Double]

predictMCLogitWithChoiceSet :: V.Vector Double -> MultiClassChoiceSet -> [Double]
predictMCLogitWithChoiceSet wtsVec (MultiClassChoiceSet altXs alts) = predictMultiClassLogit ws xs 
  where indicies = map (map xIndex) altXs
        ws       = map (map (wtsVec V.!)) indicies
        xs       = map (map xValue) altXs

predictMultiClassLogit :: [[Double]] -> [[Double]] -> [Double]
predictMultiClassLogit altWs altXs = allocateInProportionTo $ map exp zs
  where zs = zipWith sumproduct altWs altXs

predictMultiClassLogitWithPredictors :: Int -> V.Vector Double -> [Predictor] -> [Alt] -> [Double]
predictMultiClassLogitWithPredictors bits wtsVec predictors alts = predictMCLogitWithChoiceSet wtsVec choiceSet
  where hashFunc = murmurHash bits
        gobbleXs = expandedGobbleXs hashFunc predictors alts
        choiceSet = MultiClassChoiceSet gobbleXs alts


allocateInProportionTo :: [Double] -> [Double]
allocateInProportionTo !xs = sumXs `seq` (map (\x -> x / sumXs) xs)
  where sumXs = F.foldl' (+) 0 xs
