
{-# LANGUAGE PartialTypeSignatures #-}

module MachineLearning.DensityEstimation where

bigE :: Int -> _ -> _ -> _ -> [Double]
bigE n xn mu σ = do
  _ <- [n ..]
  return $ (xn - mu) ** 2 / 2 * σ**2 + log σ + 0.5 * log (2*pi)
