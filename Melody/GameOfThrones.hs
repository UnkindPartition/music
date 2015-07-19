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

      -- 9th stave
      chord ["f3", "b3b", "f4"] 3
      chord ["f3", "b3b"] 3
      chord ["f3", "d4"] 2
      chord ["f3", "e4b"] 1
      chord ["f3", "d4"] 2
      chord ["f3", "b3b"] 1

      -- 10th stave
      replicateM_ 4 pat4

      -- 11th stave
      chord ["e3b", "a3b", "c4"] 1
      mono "e3b" 1
      mono "a3b" 0.5
      mono "b3b" 0.5

      mono "c4" 1
      mono "e4b" 1
      mono "c4" 1

      chord ["e3b", "g3", "b3b"] 1
      mono "e3b" 1
      mono "g3" 0.5
      mono "a3b" 0.5

      mono "b3b" 1
      mono "e4b" 1
      mono "b3b" 1

      -- 12th stave
      chord ["f3", "a3"] 1
      mono "a3" 0.5
      mono "c4" 0.5
      mono "f4" 0.5
      mono "g4" 0.5

      mono "a4b" 1
      mono "b4b" 1
      mono "a4b" 1

      chord ["g3", "c4", "e4b", "g4"] 3
      chord ["e3b", "g3", "c4"] 3

      -- 13th stave
      chord ["c3", "f3", "a3b"] 2
      mono "c3" 0.5
      mono "f3" 0.5

      mono "a3b" 1
      mono "f3" 1
      mono "c3" 1

      chord ["e3b", "a3b", "c4", "e4b"] 3
      chord ["f3", "b3b", "d4"] 3

      -- 14th stave
      replicateM_ 3 pat4
      mono "c4" 1
      mono "g3" 1

      mono "a6b" 0.5
      mono "b6b" 0.5

      -- 15th stave
      replicateM_ 4 $ do
        mono "c7" 1
        mono "g6" 1
        mono "a6b" 0.5
        mono "b6b" 0.5


----------------------------------------------------------------------
--                           Bass
----------------------------------------------------------------------

bass_pat_cg =
      replicateM_ 12 $
        chord ["c3", "g3"] 1

bass_pat_cc =
      replicateM_ 12 $
        chord ["c2", "c3"] 1

bass_pat_gd =
      replicateM_ 12 $
        chord ["g2", "d3"] 1

bass_stave_3 = do
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

bass_pat_bf = do
      replicateM_ 2 $ do
        mono "b3b" 1
        mono "b2b" 0.5
        mono "b3b" 0.5
        mono "f2" 0.5
        mono "b2b" 0.5

bass = runMelodyM $ do
      -- 1st stave
      bass_pat_cg

      -- 2nd stave
      bass_pat_cc

      -- 3rd stave
      bass_stave_3
      
      -- 4th stave
      bass_pat_gd

      -- 5th stave
      bass_pat_bf

      replicateM_ 2 $ do
        mono "b3b" 0.5
        mono "f2" 0.5
        mono "b2b" 0.5

      replicateM_ 3 $ do
        chord ["b2b", "b3b"] 1

      -- 6th stave
      bass_pat_cc

      -- 7th stave
      bass_stave_3

      -- 8th stave
      bass_pat_gd

      -- 9th stave
      bass_pat_bf
      replicateM_ 6 $ do
        chord ["b2b", "b3b"] 1

      -- 10th stave
      bass_pat_cc

      -- 11th stave
      chord ["a1b" , "a2b"] 2
      mono "e2b" 1

      mono "a2b" 1
      mono "a1b" 1
      chord ["a1b" , "a2b"] 1

      chord ["e1b", "e2b"] 2
      mono "e1b" 1

      mono "e2b" 1
      mono "e1b" 1
      chord ["e1b", "e2b"] 2

      -- 12th stave
      chord ["f1", "f2"] 2
      mono "c3" 1
      
      mono "f3" 1
      mono "c3" 1
      mono "f2" 1

      chord ["c2", "c3"] 1
      mono "g2" 1
      mono "c3" 1

      mono "c3" 1
      mono "g2" 1
      mono "c2" 1

      -- 13th octave
      mono "e1" 1
      mono "c2" 1
      mono "f2" 1

      mono "f2" 1
      mono "c2" 1
      mono "e1" 1

      replicateM_ 3 $
        chord ["a1b", "a2b"] 1
        
      replicateM_ 3 $
        chord ["b1b", "b2b"] 1

      -- 14th stave
      replicateM_ 12 $
        chord ["c2", "c3"] 1

      -- 15 stave
      mono "c2" 12
