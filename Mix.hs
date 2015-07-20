module Mix
  (MixSettings(..), mix)
  where

import System.Random.TF.Gen
import Types
import DSL

mix :: MixSettings -> TFGen -> Melody -> Samples
mix sets@MixSettings{..} g = \case
  Empty -> Samples []
  Then m1 m2 ->
    let
      (g1, g2) = split g
    in cat (mix sets g1 m1) (mix sets g2 m2)
  Par m1 m2 ->
    let
      (g1, g2) = split g
    in add (mix sets g1 m1) (mix sets g2 m2)
  Mono note duration ->
    applyEnvelope duration .
    applyLoudness $
    runGenerator mixGenerator g (pitch note) (duration * Duration mixTempo)
  Apply sets' m -> mix sets' g m
  Mult f m -> Samples . map (* f) . getSamples $ mix sets g m
  where
    applyLoudness = 
      Samples . map (Sample . (* mixLoudness) . getSample) . getSamples
    applyEnvelope duration =
      let Samples e = runEnvelope mixEnvelope duration
      in Samples . zipWith (*) e . getSamples
