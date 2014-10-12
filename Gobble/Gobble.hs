import Data.List
import System.IO

main = do
  args   <- getArgs
  let gobArgs = processArgs args

  contents <- readFile (inputFile gobArgs)
  header <- head contents
  data   <- tail contents

  output <- onlineLogisticRegression (readTabularDataSet header data fieldDefinitions) olrParams

  writeFile "~/Documents/blah.csv" output


