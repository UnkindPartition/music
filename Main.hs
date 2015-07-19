{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-missing-signatures #-}
module Main where

import System.Random.TF.Init
import Types
import Encode
import Mix
import Melody.GameOfThrones
import Generator.Sin
import Generator.Rand
import Generator.KarplusStrong
import Envelope.ADSR

settings :: MixSettings
settings = MixSettings
  { mixGenerator = generatorKarplusStrong
  , mixTempo = 0.35
  , mixLoudness = 0.1
  , mixEnvelope = envelopeADSR 0.01 0.2 0.7 2.5
  }

main = do
  g <- initTFGen
  encodeAndWrite "test.pcm" . mix settings g $ gameOfThrones
