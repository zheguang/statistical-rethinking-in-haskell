module Sampling where

import System.Random
import Data.List
import Data.Maybe

binomial :: (Integral b, Fractional a) => b -> b -> a -> a
binomial k n p = (n `choose` k) * p ^ k * (1 - p) ^ (n - k)
  where choose n k = factorialF n / (factorialF k * factorialF (n - k))
        factorialF = fromIntegral.factorial
        factorial 0 = 1
        factorial n = product [1..n]

standardize :: [Double] -> [Double]
standardize as = map (/ sum as) as

gridApproxPosterior :: Num c => [a] -> [c] -> (a -> c) -> [c]
gridApproxPosterior pGrid prior likeF = zipWith (*) likelihood prior
  where likelihood = map likeF pGrid


grid :: (Ord a, Num a) => a -> a -> a -> [a]
grid a b step | a > b = []
              | otherwise = a : grid (a + step) b step

randsGen :: StdGen -> [(Double, StdGen)]
randsGen rng = (x, rng'):randsGen rng'
  where (x, rng') = random rng :: (Double, StdGen)


sampleContinuum :: StdGen -> [Double] -> [Double] -> Int -> [Double]
sampleContinuum rng' xs prob size =
  let bins = zip xs $ tail xs
      invcdf p = bins !! k -- xs !! k'
        where k = max (k' - 1) 0 -- k is in the range of [0,k-2]. Note it's k-2 not k-1, because maximum k' can only be `length cdf - 1`
              k' = length $ takeWhile (<p) cdf
              cdf = scanl1 (+) prob
      xGen = foldr (\x z -> randomR x (head $ map snd z) : z) [randomR (head rbins) rng] $ tail rbins
        where rbins = map invcdf unifs
              (unifs, rng) = (map fst yGen, last $ map snd yGen)
              yGen = take size $ randsGen rng'
      in map fst xGen
  -- in map (\p -> head $ randomRs (invcdf p) rng) . take 5

sampleDiscrete :: RandomGen g => g -> [Double] -> [Double] -> Int -> [Double]
sampleDiscrete rng xs prob size =
  let invcdf p = xs !! k
        where k = length $ takeWhile (<p) cdf
              cdf = scanl1 (+) prob
      in take size . map invcdf $ randoms rng

firstBy :: Ord a => (a -> a -> Bool) -> [a] -> a
firstBy f = foldl1 (\z x' -> if f x' z then x' else z)

-- Use larger `w` such as 1e-1 or 1e-2, is analogous to having wider box on top
-- of each sampled particle or point, which allows for more overlapping with
-- nearby particles. This is perhaps analogous to larger variance in the
-- Gaussian kernel.  Larger `w` results in smoother curve.
-- Use smaller `wGrid` will tend the discrete kernel towrads continuous kernel,
-- resulting in closer-to-correct density function.
data Density = Dens Double [(Double, Double)]
discreteKDE :: [Double] -> Double -> Double -> Density
discreteKDE xs w wGrid =
  let n = fromIntegral $ length xs
      x_min = firstBy (<) xs
      x_max = firstBy (>) xs
      -- w = (x_max - x_min) / n
      h = 1 / n / w
      --wGrid = w / scale
      --wGrid = 
      xGrid = grid x_min x_max wGrid
      bin :: Double -> [Double]
      bin x = takeWhile (< x + w + wGrid) $ dropWhile (<= x - wGrid) xGrid
      mass :: Double -> [(Double, Double)]
      mass x = zip (bin x) (repeat h)
      -- in xs >>= mass
      --in  sortBy (\(x, _) (x', _) -> compare x x') $ xs >>= mass
      pdf = sumByX . sortBy (\(x, _) (x', _) -> compare x x') $ xs >>= mass
        where sumByX = foldr g []
              g (x, y) [] = [(x, y)]
              g (x, y) ((x',y'):as) | x == x' = (x',y'+y):as
              g (x, y) as = (x, y):as
      in Dens wGrid pdf


massByInterval :: (Double, Double) -> [Double] -> Double
massByInterval (a, b) xs = (fromIntegral . length $ filter p xs) / fromIntegral (length xs)
  where p x = a < x && x < b


intervalByMass :: (Double, Double) -> [Double] -> (Double, Double)
intervalByMass (a, b) xs =
  let xs' = sort xs
      n = fromIntegral $ length xs
      l = floor $ a * n
      h = ceiling $ b * n
      in (xs' !! l, xs' !! h)

intervalByMassFromDensity :: (Double, Double) -> Density -> (Double, Double)
intervalByMassFromDensity (m1, m2) (Dens w pdf) =
  let pMass = map ((w*).sum.map snd) . drop 1 $ inits pdf
      pLen m = length $ takeWhile (< m) pMass
      index m = max 0 $ pLen m - 1
      in (fst $ pdf !! index m1, fst $ pdf !! index m2)


hpdi :: Double -> [Double] -> (Double, Double)
hpdi m xs =
  let xs' = sort xs
      n = fromIntegral $ length xs
      k = ceiling $ m * n
      -- nUnique as = foldr (\x z -> if x /= head z then x:z else z) (take 1 as) (tail as)
      delta as = last as - head as
      minRange = firstBy (\x x' -> delta x < delta x') . map (take k) . filter ((>=k).length) $ tails xs'
      in (head minRange, last minRange)


mode :: Density -> Double
mode (Dens _ pdf) = fst $ firstBy (\(_, y) (_, y') -> y > y') pdf

mean :: Density -> Double
mean (Dens _ pdf) = sum (map (uncurry (*)) pdf) / sum (map snd pdf)

median :: [Double] -> Double
median xs = sort xs !! n
   where n = ceiling $ fromIntegral (length xs) / 2

medianByDensity :: Density -> Double
medianByDensity (Dens w pdf) = fst . last . fromJust . find ((>=0.5).(w*).sum.map snd) $ inits pdf

lossDist :: (Double -> Double) -> [Double] -> [Double] -> Double -> Double
lossDist f post pGrid d = sum . zipWith (*) post $ map (f.subtract d) pGrid

lossAbsDist :: [Double] -> [Double] -> Double -> Double
lossAbsDist = lossDist abs

lossSqrDist :: [Double] -> [Double] -> Double -> Double
lossSqrDist = lossDist (^2)

rbinom :: (RandomGen g, Integral b) => g -> Int -> Integer -> Double -> [b]
rbinom rng size n prob =
    let ks = [0..n]
        probF = map (\x -> binomial x n prob)
        ks' = sampleDiscrete rng (map fromIntegral ks) (probF ks) size
        in map ceiling ks'

table :: Ord a => [a] -> [(a, Int)]
table xs =
  let g = group $ sort xs
      in map head g `zip` map length g


randIntsGen :: StdGen -> (Int, Int) -> [(Int, StdGen)]
randIntsGen rng (a, b) = (x, rng'):randIntsGen rng' (a, b)
  where (x, rng') = randomR (a, b) rng :: (Int, StdGen)

simulate :: StdGen -> Int -> [Double] -> (StdGen -> Double -> a) -> [a]
simulate rng size pSample predF =
  let ps = map (pSample !!) $ ks
      ksG = take size $ randIntsGen rng (0, length pSample - 1)
      (ks, rng') = (map fst ksG, snd $ last ksG)
      in map (predF rng') ps

