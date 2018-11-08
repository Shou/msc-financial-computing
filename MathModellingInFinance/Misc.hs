
{-# LANGUAGE KindSignatures, ScopedTypeVariables,
             FlexibleContexts, DataKinds
#-}

import Control.Monad (replicateM)
import Data.List
import Data.Kind
import Data.Proxy
import GHC.TypeLits

e = exp 1

data Direction = Down | Up
  deriving (Show, Eq, Ord, Enum, Bounded)

combinations :: forall a. (Bounded a, Enum a) => Int -> [[a]]
combinations size = if size > 0
                    then replicateM size values
                    else []
  where
    values = [minBound @a .. maxBound @a]

sumDirections :: [Direction] -> Int
sumDirections = sum . map f
  where
    f d = let n :: Double
              n = fromIntegral $ fromEnum d
          in round $ (n - 0.5) * 2

f :: forall d. Proxy (d :: Direction) -> _
f _ ps s d pb b r t = ps * s * d + pb * b * exp (r * t)

interestD share rate time freq =
  share * (1 + rate/freq) ** (freq * time)

interestC share rate time freq =
  share * e ** (rate * (time / freq))

findTimeC factor rate = log factor / rate

findPresentValueD target rate time freq =
  target * (1 + rate/freq) ** (-(freq * time))

findPresentValueC target rate time freq =
  target * e ** (-rate * time)

cashflowInterestD cashes times rate =
  sum $ zipWith f cashes times
    where
      freq = fromIntegral $ min (length cashes) (length times)
      f cash time = cash * (1 + rate/freq) ** time

cashflowInterestC cashes times rate =
  sum $ zipWith f cashes times
    where
      f cash time = cash * e ** (rate / time)

cashflowPresentValueC cashes times rate =
  sum $ zipWith f cashes times
    where
      f cash time = cash * e ** (-rate * time)

intrinsicValue share strike = max (share - strike) 0
timeValue shareT strike callT = undefined

