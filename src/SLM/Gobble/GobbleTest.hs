module SLM.Gobble.GobbleTest where

import SLM.Gobble.Gobble
import SLM.DataTypes
import SLM.Gobble.GobbleArgs

x1 = (Predictor "ay" (FactorValue "y")):(Predictor "ya" (FactorValue "aa")):[]
myxs = [x1]
myys = [0.0]
myws = [1.0]

args = GobbleArgs 18

my_gobble = gobble args False 42 myxs myys myws
