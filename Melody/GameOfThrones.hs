{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Melody.GameOfThrones where

import DSL
import Control.Monad

gameOfThrones = Par treble bass

----------------------------------------------------------------------
--                           Treble
----------------------------------------------------------------------

pat1 = do
      mono "g4" 1
      mono "c4" 1
      mono "e4b" 0.5
      mono "f4" 0.5

pat2 = do
      mono "g4" 1
      mono "c4" 1
      mono "e4" 0.5
      mono "f4" 0.5

pat3 = do
      mono "d4" 1
      mono "g3" 1
      mono "b3b" 0.5
      mono "c4" 0.5

pat4 = do
      mono "c4" 1
      mono "g3" 1
      mono "a3b" 0.5
      mono "b3b" 0.5

stave1 = do
      chord ["g3", "c4", "g4"] 3
      chord ["g3", "c4"] 3
      chord ["g3", "e4b"] 0.5
      chord ["g3", "f4"] 0.5
      chord ["g3", "g4"] 2
      chord ["g3", "c4"] 2
      chord ["g3", "e4b"] 0.5
      chord ["g3", "f4"] 0.5

treble = runMelodyM $ do
      -- 1st stave
      pat1
      pat1
      pat1
      pat2
      -- 2nd stave
      pat2
      pat1
      pat1
      pat1

      -- 3rd stave
      stave1

      -- 4th stave
      replicateM_ 4 pat3

      -- 5th stave
      chord ["f3", "b3b", "f4"] 3
      chord ["f3", "b3b"] 3
      chord ["f3", "e4b"] 0.5
      chord ["f3", "d4"] 0.5
      chord ["f3", "f4"] 2
      chord ["f3", "b3b"] 2
      chord ["f3", "e4b"] 0.5
      chord ["f3", "d4"] 0.5

      -- 6th stave
      replicateM_ 4 pat4

      -- 7th stave
      stave1

      -- 8th stave
      replicateM_ 4 pat3

----------------------------------------------------------------------
--                           Bass
----------------------------------------------------------------------

bass_pat_cg =
      replicateM_ 12 $
        chord ["c3", "g3"] 1

bass = runMelodyM $ do
      -- 1st stave
      bass_pat_cg

      -- 2nd stave
      replicateM_ 12 $
        chord ["c2", "c3"] 1

      -- 3rd stave
      chord ["c2", "c3"] 1
      mono "c2" 0.5
      mono "c3" 0.5
      mono "g2" 0.5
      mono "c2" 0.5

      mono "c3" 1
      mono "c2" 0.5
      mono "c3" 0.5
      mono "g2" 0.5
      mono "c2" 0.5

      replicateM_ 2 $ do
        mono "c3" 0.5
        mono "g2" 0.5
        mono "c2" 0.5

      replicateM_ 3 $ do
        chord ["c2", "c3"] 1
      
      -- 4th stave
      replicateM_ 12 $
        chord ["g2", "d3"] 1

      -- 5th stave
      replicateM_ 2 $ do
        mono "b3b" 1
        mono "b2b" 0.5
        mono "b3b" 0.5
        mono "f2" 0.5
        mono "b2b" 0.5


      replicateM_ 2 $ do
        mono "b3b" 0.5
        mono "f2" 0.5
        mono "b2b" 0.5

      replicateM_ 3 $ do
        chord ["b2b", "b3b"] 1
