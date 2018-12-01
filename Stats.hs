
{-# LANGUAGE TypeInType #-}

import Data.Proxy
import GHC.TypeLits

mean xs = sum xs / fromIntegral (length xs)

standardDeviation xs = sqrt $ sum deltas / pred n
  where
    n = fromIntegral $ length xs
    meanX = mean xs
    deltas = [ (x - meanX)**2 | x <- xs ]

normalise xs = [ (x - xMean) / xSD | x <- xs ]
  where
    xMean = mean xs
    xSD = standardDeviation xs

featureScaling xs = [ (x - xMin) / (xMax - xMin) | x <- xs ]
  where
    xMin = minimum xs
    xMax = maximum xs

expectedValue xs ps = sum $ zipWith (*) xs ps

data UnbiasedSample
class Floating b => Variance (a :: Symbol) b where
  variance :: Proxy a -> [b] -> b

instance Variance UnbiasedSample where
  variance = undefinend

-- | Unbiased sample variance
variance' xs = expectedValue xs' (replicate n (1 / fromIntegral (n - 1)))
  where
    xMean = mean xs
    xs' = flip map xs $ \x -> (x - xMean)**2
    n = length xs

sd = sqrt . variance

covariance = undefined

