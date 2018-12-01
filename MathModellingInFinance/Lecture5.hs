
import qualified Data.Vector as Vec
import qualified System.Random.MWC as MWC

facs = 1 : 2 : zipWith (\n i -> n * succ i) (tail facs) [2..]
fac n | n > 0 = facs !! pred n
      | n == 0 = 1
      | n < 0 = -(facs !! pred (abs n))

--choose n k = fac n / (fac k - fac (n - k))

bigI = fmap (round . (* 2) . (flip (-) 0.5) . Vec.head)
     . MWC.withSystemRandom
     . MWC.asGenST
     $ \gen -> MWC.uniformVector @_ @Double gen 1

{-
sou^jd^(n-j) - k

soy s u d k = s * (u**j)

psi n bigN q = sum [ 

-- Cox-Ross-Rubinstein

-- c0 = s0*psi(jmin; N, q[u]) - k / (1-r)**N * psi ? N ?
c s r k n = s * psi _ _ _ - (k / 1 + r) ** n * psi _ _ _

-}
