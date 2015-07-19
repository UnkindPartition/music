module Main where

import System.Random.TF.Init
import Types
import Encode
import Mix
import Melody.GameOfThrones
import Generator.Sin
import Generator.Rand

settings :: MixSettings
settings = MixSettings
  { mixGenerator = generatorRand
  , mixTempo = 0.4
  , mixLoudness = 0.25
  }

main = do
  g <- initTFGen
  encodeAndWrite "test.pcm" . mix settings g $ gameOfThrones
