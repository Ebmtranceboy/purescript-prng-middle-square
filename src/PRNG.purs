module PRNG where

import Prelude
import Data.Array (drop, (!!)) as Array
import Data.Array ((\\), (:), length, (..), take, concat, filter)
import Partial.Unsafe (unsafePartial)
import Data.Maybe (fromJust)
import Math (sqrt, log, cos, sin, pi)
import Data.Int (toNumber)
import Data.List (List (..), toUnfoldable)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Foldable (foldr)

nth :: forall a. Array a -> Int -> a
nth xs i =  unsafePartial fromJust $ xs Array.!! i

infixl 6 nth as !!

-- | 0 <= val < 1e4, 0 <= gen < 1e8, seed is a big odd integer 
type PRNG = {val :: Int, gen :: Int, seed :: Int}

-- | takes the 1e4 int at the middle of a 1e8 int
middle :: Int -> Int
middle nn = 
  let n0 = nn `mod` 100
      n3' = nn `mod` 1_000_000
      n3 = nn - ((nn-n3') `div` 1_000_000) * 1_000_000 
   in (n3-n0) `div` 100

-- | iterates the pseudo random generation of a value 
-- | between 0 and 9999
prng :: PRNG -> PRNG
prng {val, gen, seed} = 
  { val: middle $ (val * val + gen) `mod` 100_000_000
  , gen: (gen + seed) `mod` 100_000_000
  , seed}

unfold :: forall a. Int -> (a -> a) -> a -> List a
unfold n f x = tailRec go {accum: Nil, size: n, arg: x}
  where go {accum, size: 0, arg} = Done accum
        go {accum, size, arg} = 
          Loop { accum: Cons arg accum
               , size: size - 1
               , arg: f arg
               }  

-- | generates an array of n PRN between 0 and 9999
randoms :: Int -> PRNG -> Array Int
randoms n r = (_.val) <$> (toUnfoldable $ unfold n prng r) 

-- | generates a random permutation of [0,1,..,n-1] 
unsort :: Int -> PRNG -> Array Int
unsort n r =
  let shake :: Array Int -> PRNG -> Array Int -> Array Int
      shake [] _ ys = ys
      shake [x] _ ys = x : ys
      shake xs r' ys = 
        let r'' = prng r'
            x = xs !! (r''.val `mod` length xs)
        in shake (xs \\ [x]) r'' (x : ys)
   in shake (0..(n-1)) r []
        
chunks :: forall a. Int -> Array a -> Array (Array a)
chunks n xs = toUnfoldable $ tailRec go {accum: Nil, rest: xs}
  where go {rest, accum} = case unit of
           unit | length rest < n  -> Done accum
                | length rest == n -> Done (Cons rest accum)
                | otherwise        -> Loop { accum: Cons (take n rest) accum
                                           , rest: Array.drop n rest
                                           }            

mean :: Array Number -> Number
mean xs = (foldr add 0.0 xs) / (toNumber $ length xs)

variance :: Array Number -> Number
variance xs = 
  let square x = x*x
  in mean (square <$> xs) - square (mean xs)
  
stdev :: Array Number -> Number
stdev xs = sqrt (variance xs) 

frequency :: (Number -> Boolean) -> Array Number -> Number
frequency f xs = (toNumber $ length $ filter f xs) / (toNumber $ length xs)

type Dimension = Int
type Size = Int
type Minimum = Number
type Maximum = Number

uniforms :: Dimension -> Size -> Minimum -> Maximum -> PRNG -> Array (Array Number)
uniforms d n a b r =
  let f x = a + (b - a) * (toNumber x / 10000.0)
  in chunks d $ f <$> randoms (n*d) r

type Positive = Number

normals :: Dimension -> Size -> Number -> Positive -> PRNG -> Array (Array Number)
normals d n mu sigma rnd =
  let m = d*n + (d*n) `mod` 2
      us = uniforms 2 (m `div` 2) 0.00001 1.0 rnd
    in chunks d $ (\x -> x * sigma + mu) <$> (concat $ (\u ->
                      let r = sqrt (-2.0 * log (u !! 0))
                      in [ r * cos (2.0 * pi * (u !! 1))
                         , r * sin (2.0 * pi * (u !! 1))] ) <$> us )

exponentials :: Dimension -> Size -> Positive -> PRNG -> Array (Array Number)
exponentials d n lambda rnd =
  let us = concat $ uniforms 1 (d*n) 0.00001 1.0 rnd
    in chunks d $ (\u -> - log u / lambda) <$> us
