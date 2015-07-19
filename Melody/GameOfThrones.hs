{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Melody.GameOfThrones where

import DSL
import Control.Monad

gameOfThrones = Par treble bass

stave = checkStave 12

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
    ----------------
    stave "1 treble" $ do
      pat1
      pat1
      pat1
      pat2

    ----------------
    stave "2 treble" $ do
      pat2
      pat1
      pat1
      pat1

    ----------------
    stave "3 treble" $ do
      stave1

    ----------------
    stave "4 treble" $ do
      replicateM_ 4 pat3

    ----------------
    stave "5 treble" $ do
      chord ["f3", "b3b", "f4"] 3
      chord ["f3", "b3b"] 3
      chord ["f3", "e4b"] 0.5
      chord ["f3", "d4"] 0.5
      chord ["f3", "f4"] 2
      chord ["f3", "b3b"] 2
      chord ["f3", "e4b"] 0.5
      chord ["f3", "d4"] 0.5

    ----------------
    stave "6 treble" $ do
      replicateM_ 4 pat4

    ----------------
    stave "7 treble" $ do
      stave1

    ----------------
    stave "8 treble" $ do
      replicateM_ 4 pat3

    ----------------
    stave "9 treble" $ do
      chord ["f3", "b3b", "f4"] 3
      chord ["f3", "b3b"] 3
      chord ["f3", "d4"] 2
      chord ["f3", "e4b"] 1
      chord ["f3", "d4"] 2
      chord ["f3", "b3b"] 1

    -----------------
    stave "10 treble" $ do
      replicateM_ 4 pat4

    -----------------
    stave "11 treble" $ do
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

    -----------------
    stave "12 treble" $ do
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

    -----------------
    stave "13 treble" $ do
      chord ["c3", "f3", "a3b"] 2
      mono "c3" 0.5
      mono "f3" 0.5

      mono "a3b" 1
      mono "f3" 1
      mono "c3" 1

      chord ["e3b", "a3b", "c4", "e4b"] 3
      chord ["f3", "b3b", "d4"] 3

    -----------------
    stave "14 treble" $ do
      replicateM_ 3 pat4
      mono "c4" 1
      mono "g3" 1

      mono "a6b" 0.5
      mono "b6b" 0.5

    -----------------
    stave "15 treble" $ do
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
    --------------
    stave "1 bass" $ do
      bass_pat_cg

    --------------
    stave "2 bass" $ do
      bass_pat_cc

    --------------
    stave "3 bass" $ do
      bass_stave_3
      
    --------------
    stave "4 bass" $ do
      bass_pat_gd

    --------------
    stave "5 bass" $ do
      bass_pat_bf

      replicateM_ 2 $ do
        mono "b3b" 0.5
        mono "f2" 0.5
        mono "b2b" 0.5

      replicateM_ 3 $ do
        chord ["b2b", "b3b"] 1

    --------------
    stave "6 bass" $ do
      bass_pat_cc

    --------------
    stave "7 bass" $ do
      bass_stave_3

    --------------
    stave "8 bass" $ do
      bass_pat_gd

    --------------
    stave "9 bass" $ do
      bass_pat_bf
      replicateM_ 6 $ do
        chord ["b2b", "b3b"] 1

    ---------------
    stave "10 bass" $ do
      bass_pat_cc

    ---------------
    stave "11 bass" $ do
      chord ["a1b" , "a2b"] 2
      mono "e2b" 1

      mono "a2b" 1
      mono "a1b" 1
      chord ["a1b" , "a2b"] 1

      chord ["e1b", "e2b"] 2
      mono "e1b" 1

      mono "e2b" 1
      mono "e1b" 1
      chord ["e1b", "e2b"] 1

    ---------------
    stave "12 bass" $ do
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

    ---------------
    stave "13 bass" $ do
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

    ---------------
    stave "14 bass" $ do
      replicateM_ 12 $
        chord ["c2", "c3"] 1

    ---------------
    stave "15 bass" $ do
      mono "c2" 12
