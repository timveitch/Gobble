
module SLM.Gobble.GobbleDataWriterText where

import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as VU
import qualified Data.Double.Conversion.Text as TDC
import qualified Data.Double.Conversion.ByteString as BDC

-- import qualified Data.ByteString.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as IB

import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Text.Lazy.Encoding as E
import Data.Foldable (foldMap)
--import Data.ByteString.Lazy.Builder.ASCII

import Data.Monoid
import System.IO

data Cell = StringC String
          | IntC Int
          | DoubleC Double
  deriving( Eq, Ord, Show )

type Row   = [Cell]
type Table = [Row]

-- encodeUtf8CSV :: Table -> LC.ByteString
-- encodeUtf8CSV = toLazyByteString . renderTable

renderTable :: Table -> B.Builder
renderTable rs = mconcat [renderRow r <> B.singleton '\n' | r <- rs]

renderRow :: Row -> B.Builder
renderRow []     = mempty
renderRow (c:cs) = renderCell c <> mconcat [ B.singleton ',' <> renderCell c' | c' <- cs ]

renderCell :: Cell -> B.Builder
renderCell (IntC i)     = IB.decimal i
renderCell (DoubleC d)  = B.fromText (TDC.toExponential 4 d)
renderCell (StringC cs) = renderString cs

renderString :: String -> B.Builder
renderString cs = B.singleton '"' <> foldMap escape cs <> B.singleton '"'
  where escape '\\' = B.singleton '\\' <> B.singleton '\\'
        escape '\"' = B.singleton '\\' <> B.singleton '\"'
        escape c    = B.singleton c

writeWeightsToFile :: FilePath -> VB.Vector [(Int,Double)] -> IO ()
writeWeightsToFile filename weights = do
    let dim = VB.length weights
    let indexedWeights = VB.zipWith (\a b -> (a,b)) (VB.enumFromN 0 dim) weights
    let tables = VB.imap (\varIndex weightsList -> 
               map (\(weightIndex,weight) -> [IntC varIndex, IntC weightIndex, DoubleC weight]) weightsList 
            ) weights
    let builders = VB.map renderTable tables
    let bigBuilder = (IB.decimal dim) <> B.singleton '\n' <> mconcat (VB.toList builders)
    let bs = E.encodeUtf8 . B.toLazyText $ bigBuilder
    putStrLn (take 100 (show bs))
    LC.writeFile filename bs
