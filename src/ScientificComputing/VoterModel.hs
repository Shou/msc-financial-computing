{-# LANGUAGE TypeApplications, BangPatterns, PartialTypeSignatures #-}

module ScientificComputing.VoterModel where

import qualified Data.Matrix.Static as Mat
import qualified System.Random.MWC as MWC


computeRho s = rho'
    where
      (r, c) = (undefined s, undefined s)
      sl = undefined
      rho = undefined
      rho' = undefined

voterModel :: _ -> _ -> IO (_, _, _)
voterModel l steps = do
  xs <- MWC.withSystemRandom
      . MWC.asGenST
      $ undefined -- \gen -> MWC.uniformVector @_ @Double gen undefined
  pure undefined
    where
      s = 2 * undefined
      m = replicate steps 0
      rho = replicate steps 0
