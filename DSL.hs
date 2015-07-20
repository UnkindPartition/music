module DSL where

import Data.Monoid
import Control.Monad.Trans.Writer
import Control.Applicative
import Types

data MixSettings = MixSettings
  { mixGenerator :: Generator
  , mixTempo :: Double
  , mixLoudness :: Double -- from 0 to 1
  , mixEnvelope :: Envelope
  }
instance Show MixSettings where show _ = "<settings>"

data Melody
  = Par Melody Melody
  | Then Melody Melody
  | Mono Note Duration
  | Empty
  | Apply MixSettings Melody
  deriving Show

instance Monoid Melody where
  mempty = Empty
  mappend = Then

melodyDuration :: Melody -> Duration
melodyDuration = \case
  Par m1 m2 -> max (melodyDuration m1) (melodyDuration m2)
  Then m1 m2 -> melodyDuration m1 + melodyDuration m2
  Mono _ d -> d
  Empty -> 0
  Apply _ m -> melodyDuration m

----------------------------------------------------------------------
--                           Monadic DSL
----------------------------------------------------------------------

newtype MelodyM a = MelodyM (Writer Melody a)
  deriving (Functor, Applicative, Monad)

mono :: Note -> Duration -> MelodyM ()
mono note dur = MelodyM (tell $ Mono note dur)

chord :: [Note] -> Duration -> MelodyM ()
chord notes dur = MelodyM (tell $ foldr Par Empty $ map (flip Mono dur) notes)

runMelodyM :: MelodyM () -> Melody
runMelodyM (MelodyM a) = execWriter a

checkStave :: Duration -> String -> MelodyM () -> MelodyM ()
checkStave duration name a@(runMelodyM -> m) =
  let actualDuration = melodyDuration m in
  if (actualDuration == duration)
    then a
    else
      error $ "Duration of stave " ++ name ++ " is " ++ show actualDuration
