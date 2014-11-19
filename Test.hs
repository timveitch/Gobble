
module Test where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Control.Monad.Primitive


go :: Int -> Int -> IO (V.Vector (V.Vector Double))
go dim1 dim2 = do
  singleVec <- VM.replicate dim2 0
  multiVec  <- V.generateM dim1 (\i -> VM.replicate dim2 0)
  --  VM.MVector RealWorld (VM.MVector RealWorld Double)

  --V.mapM_ (\index -> VM.write multiVec index VM.replicate dim2 0) [0..(dim1-1)]
  
  VM.write (V.head multiVec) 1 2
  --return multiVec
  multiVecFrozen <- V.mapM (V.unsafeFreeze) multiVec
  return (multiVecFrozen)
  --V.unsafeFreeze multiVecFrozen
  
