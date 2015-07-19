module Generator.Sin where

import Types

generatorSin :: Generator
generatorSin = Generator $ \_ (Freq freq) (Duration duration) ->
  let
    samples_in_period = 44100 / freq
    periods = duration * freq
  in
    Samples $ concat $ replicate (round periods) $
      [ Sample $ sin (2 * pi * fromIntegral n / samples_in_period)
      | n <- [0..round samples_in_period - 1]
      ]
