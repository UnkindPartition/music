module Mix where

import System.Random.TF.Gen
import Types
import DSL

data MixSettings = MixSettings
  { mixGenerator :: Generator
  , mixTempo :: Double
  , mixLoudness :: Double -- from 0 to 1
  }

mix :: MixSettings -> TFGen -> Melody -> Samples
mix MixSettings{..} = go where
  go g = \case
    Empty -> Samples []
    Then m1 m2 ->
      let
        (g1, g2) = split g
      in cat (go g1 m1) (go g2 m2)
    Par m1 m2 ->
      let
        (g1, g2) = split g
      in add (go g1 m1) (go g2 m2)
    Mono note duration ->
      applyLoudness $
      runGenerator mixGenerator g (pitch note) (duration * Duration mixTempo)
    where
      applyLoudness = 
        Samples . map (Sample . (* mixLoudness) . getSample) . getSamples

