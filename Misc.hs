
fac 1 = 1
fac n = n * fac (n-1)

nChooseK' n k = foldr @[] f 1 [1..k]
  where
    n' = fromIntegral n
    f i p = p * (n' + 1 - i) / i

