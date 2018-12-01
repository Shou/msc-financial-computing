{-# LANGUAGE TypeApplications, BangPatterns, PartialTypeSignatures #-}


import qualified Data.Matrix.Static as Mat
import qualified System.Random.MWC as MWC


computeRho s = rho'
    where
      (r, c) = (Mat.numRows s, Mat.numCols s)
      sl = Mat.setSize
      rho = undefined
      rho' = undefined

voterModel :: _ -> _ -> (_, _, _)
voterModel l steps = do
  xs <- MWC.withSystemRandom
      . MWC.asGenST
      $ \gen -> MWC.uniformVector @_ @Double gen n
    where
      s = 2 * undefined
      m = replicate steps 0
      rho = replicate steps 0
