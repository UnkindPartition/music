module Envelope.ADSR where

import Data.Tuple.Homogenous
import Data.Foldable as F
import Control.Applicative
import Types

envelopeADSR
  :: Duration
  -> Duration
  -> Sample
  -> Duration
  -> Envelope
envelopeADSR attackDur decayDur sustainLev releaseDur = Envelope $ \totalDur ->
  let
    sustainDur = totalDur - (attackDur + decayDur + releaseDur)
    durations = Tuple4 (attackDur, decayDur, sustainDur, releaseDur)
    nsamples = durationToNSamples <$> durations

    attackPart ns = [ Sample $ fromIntegral n / fromIntegral ns | n <- [0..ns-1]]
    decayPart ns = [ sustainLev + (1 - sustainLev) * Sample (fromIntegral (ns-n) / fromIntegral ns) | n <- [0..ns-1]]
    sustainPart ns = replicate ns sustainLev
    releasePart ns = [ sustainLev * Sample (fromIntegral (ns-n) / fromIntegral ns) | n <- [0..ns-1]]
  in
    Samples $ take (durationToNSamples totalDur) . fold $
      Tuple4 (attackPart, decayPart, sustainPart, releasePart) <*> nsamples
