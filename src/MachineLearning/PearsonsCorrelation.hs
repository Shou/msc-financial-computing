
module MachineLearning.PearsonsCorrelation where

correlation xs ys = xys / rootedSquareSums
    where
      meanX = sum xs / fromIntegral (length xs)
      meanY = sum ys / fromIntegral (length ys)
      xs' = map (+ (-meanX)) xs
      ys' = map (+ (-meanY)) ys
      xys = sum $ zipWith (*) xs' ys'
      rootedSquareSums = sqrt $ sum [x**2|x<-xs'] * sum [y**2|y<-ys']

