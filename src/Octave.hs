
{-# LANGUAGE TypeApplications #-}

module Octave where

import Control.Monad (replicateM)
import System.Random (randomRIO)

rand (y, x) = replicateM x
            $ replicateM y
            $ randomRIO @Double (0, 1)

