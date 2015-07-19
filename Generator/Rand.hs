module Generator.Rand where

import System.Random.TF.Instances
import Types

generatorRand :: Generator
generatorRand = Generator $ \g (Freq freq) (Duration duration) ->
  let
    samples_in_period = 44100 / freq
    periods = duration * freq
  in
    Samples $ concat $ replicate (round periods) $
      take (round samples_in_period) $ randoms g
