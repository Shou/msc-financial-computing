
{-# LANGUAGE TypeInType, MultiParamTypeClasses, AllowAmbiguousTypes, ScopedTypeVariables #-}

import Data.Proxy
import GHC.TypeLits


type a |= b = b

data Biased
data Unbiased

class Variance a where
  variance :: forall a b. Floating b => [b] -> a |= b
  covariance :: forall a b. Floating b => [b] -> [b] -> a |= b

instance Variance Unbiased where
  variance xs = varianceBuilder xs (-1)
  covariance xs ys = covarianceBuilder xs ys (-1)

instance Variance Biased where
  variance xs = varianceBuilder xs 0
  covariance xs ys = covarianceBuilder xs ys 0


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

varianceBuilder xs bias =
  expectedValue xs' (replicate n (1 / fromIntegral (bias + n)))
    where
      xMean = mean xs
      xs' = flip map xs $ \x -> (x - xMean)**2
      n = length xs

covarianceBuilder xs ys bias =
  sum xys / fromIntegral (bias + n)
    where
      xMean = mean xs
      yMean = mean ys
      xs' = map (+ negate xMean) xs
      ys' = map (+ negate yMean) ys
      xys = zipWith (*) xs' ys'
      n = length xys

sd :: forall a b. (Variance a, Floating b) => [b] -> b
sd = sqrt . variance @a

corr :: forall a b. (Variance a, Floating b) => [b] -> [b] -> b
corr xs ys = covariance @a xs ys / (sqrt $ variance @a xs * variance @a ys)

correlation xs ys = xys / rootedSquareSums
    where
      meanX = sum xs / fromIntegral (length xs)
      meanY = sum ys / fromIntegral (length ys)
      xs' = map (+ negate meanX) xs
      ys' = map (+ negate meanY) ys
      xys = sum $ zipWith (*) xs' ys'
      rootedSquareSums = sqrt $ sum [x**2|x<-xs'] * sum [y**2|y<-ys']

