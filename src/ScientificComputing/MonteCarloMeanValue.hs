
{-# LANGUAGE TypeApplications, BangPatterns, PartialTypeSignatures #-}

module ScientificComputing.MonteCarloMeanValue where

import Control.Monad
import qualified Data.Vector as Vec
import qualified System.Random.MWC as MWC

weirdMeanValue (a, b) n = do
  let !d = b-a
  xs <- MWC.withSystemRandom
      . MWC.asGenST
      $ \gen -> MWC.uniformVector @_ @Double gen n

  let f :: Vec.Vector Double -> Vec.Vector Double
      f ts = let ts' = do
                   t <- ts
                   return $ 3 - (t * d + a)

             in do z <- Vec.zipWith (*) ts ts'
                   return $ (sin (1.0 / z)) ** 2

  let n' = fromIntegral n
      vs = f xs
  return $ d / n' * Vec.sum vs

