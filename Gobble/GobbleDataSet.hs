type ClassLabel = String

type ParameterIndex = Int

data GobbleX = GobbleX {
    xIndex  :: Int
  , xWeight :: Double
  , xValue  :: Double
} deriving (Show)

type GobbleXList = [GobbleX]

data GobbleObservation = GobbleObservation {
    y   :: Double
  , xs  :: GobbleXList
} deriving (Show)

data GobbleTrainingSet = GobbleTrainingSet { 
  gobbleObservations = [GobbleObservation]
}
