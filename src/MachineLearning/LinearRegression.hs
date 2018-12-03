
module MachineLearning.LinearRegression where

xs :: [Int]
xs = [1 .. 60]
ys = do
  x <- xs
  pure $ x^2+3

f :: Int -> Int -> Int
f w x =
  let y = w * x
  in undefined

