{-# LANGUAGE TypeApplications #-}

import System.Random
import Control.Monad


-- Expected value = sum of outcomes multiplied by their respective probabilities
-- If their probaility is the same, sum outcomes and multiply by the probability
expectedValue3 = sum @[] [1,2,3,4,5,6] / 6
expectedValue2 = 1/2 * expectedValue3 + 1/6 * sum @[] [4,5,6]
expectedValue1 = 2/3 * expectedValue2 + 1/6 * sum @[] [5,6]

dice = randomRIO @Int (1,6)

game :: Int -> IO Int
game len | len < 1 = pure 0
game len = do
  if len == 1
  then dice
  else do
    n <- dice
    if n < 4 then game (pred len) else pure n

estExpValue :: Int -> Int -> IO Double
estExpValue games tests = (/ fromIntegral tests) . fromIntegral . sum <$> replicateM tests (game games)

