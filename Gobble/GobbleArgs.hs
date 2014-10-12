data GobbleArgs = GobbleArgs {
    inputFile :: FilePath
} deriving (Show)

ParseArgs :: [String] -> GobbleArgs
ParseArgs (inputFile:[]) = GobbleArgs inputFile




