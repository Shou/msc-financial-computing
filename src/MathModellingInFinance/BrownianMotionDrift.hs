
module MathModellingInFinance.BrownianMotionDrift where

-- | Wiener process AKA brownian motion
-- Increments over 0 are normally distributed:
-- W(t) - W(s) ~ N(0, t - s)
wiener 0 = 0
wiener t = undefined

-- | x 0 is the initial price of the stock at time 0, arbitrarily 3 here
-- | Size of fluctuations specified by `sigma`
x 0 _ _ = 3
x t m sigma = x (pred t) m sigma + m * t + sigma * wiener t

-- X(t) ~ N(X(0) + m * t, sigma^2 * t)
-- E[X(t)] = X(0) + m*t
-- Var(X(t)) = sigma^2 * t


-- Geometric Brownian motion
-- | GBM is a good model for share prices, see Topic 2 slide 14 where we analysed
-- _actual_ price data with tau = 1 day.
-- mu = expected growth rate of Y(t)
-- E[Y(t)] = Y(0) * exp(mu * t) i.e. mu = m + sigma^2 / 2
gbm 0 sigma m = undefined
gbm t sigma m = gbm 0 sigma m * exp(m * t + sigma * wiener t)

-- Approximating GBM with a lattice random walk
-- --------------------------------------------
--
-- GBM: drift mu and volatility sigma, starting at share0 at time = 0,
-- This can eb approximated by...
--
-- Lattice random walk: starts at share0 at time 0
-- * Let S be time period corresponding to each timestep of the lattice
--   (choose S to be small).
-- * At each timestep, move
--   * either up by a multiplicative factor u = e^(sigma * sqrt(delta))
--     with probability Pu = 1/2 * (1 + (m * sqrt(delta)) / sigma)
--     where m = mu - sigma^2 / 2
--   * or down by a multiplicative fator d = e^(-sigma * sqrt(delta))
--     with probability Pd = 1/2 * (1 - (m * sqrt(delta)) / sigma)
--   Smaller delta → closer the approximation to GBM.
--   (Mathematically GBM is the "limit" as delta → 0.
--   (See printed notes for non-rigorous proof)
