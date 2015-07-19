module Melody.GameOfThrones where

import DSL
import Control.Monad

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

gameOfThrones = runMelodyM $ do
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
