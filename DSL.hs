module DSL where

import Data.Monoid
import Control.Monad.Trans.Writer
import Control.Applicative
import Types

data Melody
  = Par Melody Melody
  | Then Melody Melody
  | Mono Note Duration
  | Empty
  deriving Show

instance Monoid Melody where
  mempty = Empty
  mappend = Then

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
