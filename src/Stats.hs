
{-# LANGUAGE TypeInType, MultiParamTypeClasses, AllowAmbiguousTypes,
             ScopedTypeVariables, TypeApplications, TypeOperators,
             PartialTypeSignatures
#-}

module Stats
  ( module Stats
  , module Plot
  ) where

import Control.Monad (replicateM)
import Data.List (nub)
import Data.Monoid ((<>))
import Data.Proxy
import GHC.TypeLits
import System.Random (randomRIO)

import qualified Graphics.Gnuplot.Simple as Plot


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


randomList 0 _ = mempty
randomList n (nMin, nMax) = do
  rs <- fmap nub . replicateM n $ randomRIO (nMin, nMax)
  let rsLen = length rs
  if rsLen < n
    then (<>) <$> pure rs <*> randomList (n - rsLen) (nMin, nMax)
    else pure rs

dice :: IO Int
dice = head <$> randomList 1 (1,6)

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
  expectedValue [x**2|x<-xs] ps - xMean**2
    where
      ps = replicate n (1 / fromIntegral (bias + n))
      xMean = mean xs
      n = length xs

covarianceBuilder xs ys bias =
  sum xys / fromIntegral (bias + n)
    where
      xMean = mean xs
      yMean = mean ys
      xys = zipWith (*) [x-xMean|x<-xs] [y-yMean|y<-ys]
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

gaussian xs = map formula xs
  where
    xMean = mean xs
    xSD = standardDeviation xs
    formula x = exp (negate (((x - xMean)**2) / (2 * xSD**2))) / (xSD * sqrt (2*pi))

det :: [[Double]] -> Double
det matX
  | length matX == 2 = mat2 matX
  | otherwise = undefined
    where
      mat2 :: [_] -> _
      mat2 [] = 0
      mat2 [[a, b], [c, d]] = a*d-b*c

normDist xs = map (normDistX xVar xMean) xs
  where
    xVar = variance @Unbiased xs
    xMean = mean xs

normDistX xVar xMean x = (1 / sqrt (2 * pi * xVar)) * exp(-((x - xMean)**2 / (2 * xVar)))

probAnd a b = a * b
probCond a b = probAnd a b / a

em :: Int -> Int -> [Double] -> IO _
em k i xs = do
    let n = length xs
        vars = undefined
        ps = replicate k (1 / fromIntegral k)
    mus <- map (xs !!) <$> replicateM k (randomRIO (0, n - 1))
    em' xs ps mus vars i
  where
    em' xs ps mus vars 0 = undefined
    em' xs ps mus vars i = undefined
    eStep xs ps mus vars ki =
      let gs = normDist xs
          pb = ps !! (ki - 1)
          pa = 1 - pb
          bs = let a = undefined; b = undefined;
               in [ (gi * b) / (gi * b + gi * a) | gi <- gs ]
          as = map (1 -) bs
          bMean = expectedValue bs xs / sum bs
          bVar = expectedValue bs [(x-bMean)**2|x<-xs] / sum bs
          aMean = expectedValue as xs / sum as
          aVar = expectedValue as [(x-aMean)**2|x<-xs] / sum as
      in undefined
    mStep = undefined
