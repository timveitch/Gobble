
{-# LANGUAGE BangPatterns #-}

module SLM.Gobble.GobbleM where

import qualified Data.IntMap as IM
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Vector as VB
import qualified Data.Vector.Mutable as VBM

import Data.Maybe
import Control.Monad

import SLM.DataTypes
import SLM.Gobble.GobbleDataTypes
import SLM.Gobble.GobbleArgs
import Control.Applicative
import qualified Data.Foldable as F
import Control.Monad.Primitive

-- PUBLIC INTERFACE
-- gobble :: GobbleArgs -> [GobbleExample] -> [Double] -> [Double] -> IO (V.Vector Double)
-- gobble args xs ys ws = gobbleGobble args (map (map (makeGobbleX hashFunc)) xs) ys ws
--   where hashFunc = murmurHash (bits args)
-- 
-- gobbleMultiClass :: GobbleArgs -> [[Predictor]] -> [Alternative] -> [[Alternative]] -> [Double] -> IO (V.Vector Double)


-- INTERNAL INTERFACE
gobble :: GobbleArgs -> [GobbleExample] -> [Double] -> [Double] -> IO (V.Vector Double)
gobble args xs ys ws = do
    wtsVec <- VM.replicate size 0
    gsVec  <- VM.replicate size 1

    zipWithM_ (processObs wtsVec gsVec gradFunc predFunc) xs ys
    V.unsafeFreeze wtsVec

  where size     = vecSize (bits args)
        xys      = zipWith (\x y -> (x,y)) xs ys
        gradFunc = gobbleGradientLogistic
        predFunc = logistic

vecSize :: Int -> Int
vecSize bits = 2^bits

gobbleMulti :: GobbleArgs -> [GobbleExample] -> [VB.Vector Double] -> [Double] -> IO (VB.Vector (V.Vector Double))
gobbleMulti args xs multiYs ws = do
    multiWtsVec <- VB.replicateM dim (VM.replicate size 0)
    multiGsVec  <- VB.replicateM dim (VM.replicate size 1.0)

    zipWithM_ (\x ys -> VB.sequence_ (VB.zipWith3 (\y wtsVec gsVec -> processObs wtsVec gsVec gradFunc predFunc x y) ys multiWtsVec multiGsVec)) xs multiYs
    VB.mapM (V.unsafeFreeze) multiWtsVec
  where size = vecSize (bits args)
        dim  = VB.length (head multiYs)
        gradFunc = gobbleGradientLogistic
        predFunc = logistic

retrieveConst :: VM.MVector RealWorld Double -> Maybe Int -> IO (Double)
retrieveConst wtsVec (Just index) = VM.read wtsVec index
retrieveConst _ _ = return (0.0)

processObs :: VM.MVector RealWorld Double -> VM.MVector RealWorld Double -> GradientFunc -> PredictionFunc -> GobbleExample -> Double -> IO ()
processObs wtsVec gsVec !gradFunc !predFunc !(GobbleExample indices maybeXs maybeConstIndex) !y = do
    ws            <- V.mapM (VM.read wtsVec) indices
    gsums         <- V.mapM (VM.read gsVec)  indices
    const         <- retrieveConst wtsVec maybeConstIndex

    let z          = computeZ ws maybeXs const
    let prediction = predFunc z
    let error      = prediction - y
    let gradients  = computeGradients gradFunc error maybeXs

    let newWs      = computeNewWeights ws gradients gsums
    let newGSums   = computeNewGSums   gsums gradients

    V.zipWithM_ (\newW index    -> VM.write wtsVec index newW) newWs indices
    V.zipWithM_ (\newGSum index -> VM.write gsVec  index newGSum) newGSums indices

    updateConstWeight wtsVec gradFunc const error maybeConstIndex

updateConstWeight :: VM.MVector RealWorld Double -> GradientFunc -> Double -> Double -> Maybe Int -> IO ()
updateConstWeight wtsVec gradFunc startConst error (Just constIndex) = do
  let gradient = gradFunc error 1.0
  let newWeight = startConst + adaptiveWeightInc gradient 1.0
  VM.write wtsVec constIndex newWeight
updateConstWeight _ _ _ _ Nothing = return ()


computeZ :: V.Vector Double -> Maybe (V.Vector Double) -> Double -> Double
computeZ ws maybeXs const = featureSum + const
  where featureSum = case maybeXs of Nothing -> V.foldl' (+) 0.0 ws
                                     Just xs -> V.foldl' (+) 0.0 (V.zipWith (*) ws xs)

computeGradients :: (Double -> Double -> Double) -> Double -> Maybe (V.Vector Double) -> Either Double (V.Vector Double)
computeGradients gradFunc !error Nothing = Left (gradFunc error 1)
computeGradients gradFunc !error (Just xs) = Right (V.map (gradFunc error) xs)

computeNewWeights :: V.Vector Double -> Either Double (V.Vector Double) -> V.Vector Double -> V.Vector Double
computeNewWeights startWs (Left gradient) gradientSums = V.zipWith calcNewW startWs gradientSums
  where calcNewW startW gradientSum = startW + (adaptiveWeightInc gradient gradientSum)
computeNewWeights startWs (Right gradients) gradientSums = V.zipWith3 calcNewW' startWs gradients gradientSums
  where calcNewW' startW gradient gradientSum = startW + adaptiveWeightInc gradient gradientSum

computeNewGSums :: V.Vector Double -> Either Double (V.Vector Double) -> V.Vector Double
computeNewGSums startGSums (Left gradient) = absGradient `seq` (V.map (+absGradient) startGSums)
  where absGradient = abs gradient
computeNewGSums startGSums (Right gradients) = V.zipWith (\gsum gradient -> gsum + abs gradient) startGSums gradients

-- processObs_ :: VM.MVector RealWorld Double -> VM.MVector RealWorld Double -> GradientFunc -> Double -> (GobbleExample,Double) -> IO ()
-- processObs_ wtsVec gsVec gradFunc prediction (gobbleXs,y) = do
--  prediction `seq` mapM_ (processX wtsVec gsVec gradFunc prediction y) gobbleXs

-- processX :: VM.MVector RealWorld Double -> VM.MVector RealWorld Double -> GradientFunc -> Double -> Double -> GobbleX -> IO ()
-- processX wtsVec gsVec gradFunc prediction actual (GobbleX index x) = do
--    startWeight     <- VM.read wtsVec index
--    startGS         <- VM.read gsVec  index
--    let weightInc    =  adaptiveWeightInc gradient startGS
--    let newWeight    = startWeight + weightInc
--    let newGS        = startGS + gsInc

--    newWeight `seq` VM.write wtsVec index newWeight
--    newGS     `seq` VM.write gsVec  index newGS
--  where
--    gradient    = gradFunc x actual prediction
--    gsInc       = abs gradient

adaptiveWeightInc :: Double -> Double -> Double
adaptiveWeightInc gradient gradientSum = -gradient * alpha / (gradientSum ** power)

-- GOBBLE MODEL UPDATE
-- gobbleUpdateOnObs :: GradientFunc -> PredictionFunc -> [GobbleX] -> Double -> GobbleModel -> GobbleModel
--gobbleUpdateOnObs gradientFunc predictionFunc xs actual inputModel = F.foldl' f inputModel xs
  --where prediction = predictionFunc inputModel xs
  --      f model x = gobbleUpdateOnSinglePredictor gradientFunc x actual prediction model

alpha = 0.10
power = 0.6

-- PREDICTION
type PredictionFunc   = Double -> Double

logistic :: Double -> Double
logistic z = 1.0 / (1.0 + exp(-z))

inverseLogistic :: Double -> Double
inverseLogistic probability = -(log (1.0 / probability - 1))

predictExample :: V.Vector Double -> GobbleExample -> Double
predictExample !wtsVec !(GobbleExample indices maybeXs maybeConstIndex) = computeZ ws maybeXs const
  where ws    = V.map (wtsVec V.!) indices
        const = case maybeConstIndex of (Just index) -> wtsVec V.! index
                                        Nothing      -> 0.0

predictExampleLogit :: V.Vector Double -> GobbleExample -> Double
predictExampleLogit wtsVec example = logistic $ (predictExample wtsVec example)

-- GRADIENTS
type GradientFunc = Double -> Double -> Double

gobbleGradientLogistic :: Double -> Double -> Double
gobbleGradientLogistic error x = error * x

-- HELPERS
nonZeroWeights :: V.Vector Double -> [(Int,Double)]
nonZeroWeights wtsVec = V.toList $ V.filter nonZeroWeight (V.imap (\index weight-> (index,weight)) wtsVec)

nonZeroWeight :: (Int,Double) -> Bool
nonZeroWeight (_,w) = w /= 0

makeFromNonZeroWeights :: Int -> [(Int,Double)] -> V.Vector Double
makeFromNonZeroWeights bits nonZeroWeights = (V.replicate (vecSize bits) 0.0) V.// nonZeroWeights
