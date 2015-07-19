module Generator.KarplusStrong where

import System.Random.TF.Instances
import Data.Sequence (Seq, (|>), ViewR(..), ViewL(..), viewl, viewr)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Types

generatorKarplusStrong :: Generator
generatorKarplusStrong = Generator $ \g freq duration ->
  let
    samples_in_period = round $ 44100 / freq - 1/2
    initial = Seq.fromList $ take samples_in_period $ randoms g
    nsamples = durationToNSamples duration
  in Samples $ take nsamples $ concatMap toList $ initial : loop initial

loop :: Seq Sample -> [Seq Sample]
loop buf =
  let
    -- length p-1
    buf1 = Seq.zipWith avg buf (Seq.drop 1 buf)
    buf2 = buf1 |> avg (seqLast buf) (seqHead buf1)
  in buf2 : loop buf2

seqHead :: Seq a -> a
seqHead s = case viewl s of
  x :< _ -> x
  _ -> error "seqHead: empty seq"

seqLast :: Seq a -> a
seqLast s = case viewr s of
  _ :> x -> x
  _ -> error "seqLast: empty seq"

avg :: Sample -> Sample -> Sample
avg x y = (x+y)/2
