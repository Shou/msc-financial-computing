
import Data.Bits (xor)

-- [0,1,0,1,1,1,0,1,0,0]

rule90 :: [Int] -> [Int]
rule90 xs = 0 : rule90' xs
  where
    rule90' (a:mid:b:[]) = [xor a b, 0]
    rule90' (a:mid:b:rest) = xor a b : rule90' (mid:b:rest)

v0 :: [Int]
v0 = replicate 100 0 ++ [1] ++ replicate 100 0

bigV = foldr f [v0] ([1..99] :: [Int])
  where
    f _ state = state ++ [rule90(last state)] 

