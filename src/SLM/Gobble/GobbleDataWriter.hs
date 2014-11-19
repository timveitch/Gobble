
module SLM.Gobble.GobbleDataWriter where

import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as VU
import qualified Data.Double.Conversion.Text as TDC
import qualified Data.Double.Conversion.ByteString as BDC

import qualified Data.ByteString.Lazy.Builder as B
import qualified Data.ByteString.Lazy.Char8 as LC
-- import Data.Text.Encoding as E
import Data.Foldable (foldMap)
import Data.ByteString.Lazy.Builder.ASCII

import Data.Monoid
import System.IO

data Cell = StringC String
          | IntC Int
          | DoubleC Double
  deriving( Eq, Ord, Show )

type Row   = [Cell]
type Table = [Row]

encodeUtf8CSV :: Table -> LC.ByteString
encodeUtf8CSV = toLazyByteString . renderTable

renderTable :: Table -> B.Builder
renderTable rs = mconcat [renderRow r <> B.charUtf8 '\n' | r <- rs]

renderRow :: Row -> B.Builder
renderRow []     = mempty
renderRow (c:cs) = renderCell c <> mconcat [ B.charUtf8 ',' <> renderCell c' | c' <- cs ]

renderCell :: Cell -> B.Builder
renderCell (IntC i)     = intDec i
renderCell (DoubleC d)  = B.byteString (BDC.toExponential 4 d)
renderCell (StringC cs) = renderString cs

renderString :: String -> B.Builder
renderString cs = B.charUtf8 '"' <> foldMap escape cs <> B.charUtf8 '"'
  where escape '\\' = B.charUtf8 '\\' <> B.charUtf8 '\\'
        escape '\"' = B.charUtf8 '\\' <> B.charUtf8 '\"'
        escape c    = B.charUtf8 c

writeWeightsToFile :: FilePath -> VB.Vector [(Int,Double)] -> IO ()
writeWeightsToFile filename weights = do
    let dim = VB.length weights
    let indexedWeights = VB.zipWith (\a b -> (a,b)) (VB.enumFromN 0 dim) weights
    let tables = VB.imap (\varIndex weightsList -> 
               map (\(weightIndex,weight) -> [IntC varIndex, IntC weightIndex, DoubleC weight]) weightsList 
            ) weights
    let builders = VB.map renderTable tables
    let bigBuilder = (intDec dim) <> B.charUtf8 '\n' <> mconcat (VB.toList builders)
    -- let bs = E.encodeUtf8 . TB.toLazyText $ bigBuilder
    let bs = toLazyByteString bigBuilder
    --putStrLn (take 100 (show bs))
    LC.writeFile filename bs

writePredictionsToFile :: FilePath -> [VB.Vector Double] -> IO ()
writePredictionsToFile filename preds = do
  let predsList = map (\predsVec -> VB.toList predsVec) preds
  let predsTable = map (map DoubleC) predsList
  let builder    = renderTable predsTable
  let bs = toLazyByteString builder
  LC.writeFile filename bs
