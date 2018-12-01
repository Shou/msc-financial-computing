
xs = [1 .. 60]
ys = do
  x <- xs
  pure $ x^2+3

f w x =
  let y = w * x
  in 
