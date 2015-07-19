module Types where

import System.Random.TF.Gen
import System.Random.TF.Instances
import Data.Function (on)
import Data.Int
import Data.String

newtype Freq = Freq Double
  deriving (Eq, Ord, Real, Num, Fractional, RealFrac, Show)

newtype Sample = Sample { getSample :: Double }
  deriving (Eq, Ord, Real, Num, Fractional, Show)

instance Random Sample where
  random g =
    let (w32, g') = next g
        sample = fromIntegral (fromIntegral w32 :: Int32) / fromIntegral (maxBound :: Int32)
    in (sample, g')
  randomR = error "randomR undefined for Sample"

newtype Samples = Samples { getSamples :: [Sample] }
  deriving Show

newtype Duration = Duration Double
  deriving (Eq, Ord, Real, Num, Fractional, Show)

newtype Note = Note String
  deriving (Show, IsString)

newtype Generator = Generator { runGenerator :: TFGen -> Freq -> Duration -> Samples }

newtype Envelope = Envelope { runEnvelope :: Duration -> Samples }

pitch :: Note -> Freq
pitch (Note note0) | (letter : octave : mbModifier) <- note0 =
  let
    note 'c' = 0
    note 'd' = 2
    note 'e' = 4
    note 'f' = 5
    note 'g' = 7
    note 'a' = 9
    note 'b' = 11
    note _ = badNote

    modifierShift =
      case mbModifier of
        ['b'] -> -1
        ['#'] ->  1
        [] -> 0
        _ -> badNote
    shift =
      note letter + (read [octave]) * 12 + modifierShift
      - 9 - 4*12 -- a4
  in
     Freq $ exp (shift * log 2 / 12) * 440

  | otherwise = badNote

  where
    badNote = error $ "Bad note: " ++ note0

cat :: Samples -> Samples -> Samples
cat = (Samples .) . (++) `on` getSamples

add :: Samples -> Samples -> Samples
add = (Samples .) . go `on` getSamples
  where
    go (s1:ss1) (s2:ss2) =
      let !s = (s1+s2)
      in s : go ss1 ss2
    go [] ss = ss
    go ss [] = ss

sampleRate :: Double
sampleRate = 44100

durationToNSamples :: Duration -> Int
durationToNSamples (Duration dur) = round $ sampleRate * dur
