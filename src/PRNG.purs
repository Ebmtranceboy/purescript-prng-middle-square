module PRNG where

import Prelude
import Data.Array (drop, (!!)) as Array
import Data.Array ((\\), (:), length, (..), take, concat, filter, mapWithIndex)
import Partial.Unsafe (unsafePartial)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Math (sqrt, log, cos, sin, pi)
import Data.Int (toNumber)
import Data.Map (Map, empty, insert, isEmpty, lookup, values)
import Data.Map (filter) as Map
import Data.Map.Internal (keys)
import Data.List (List (..), toUnfoldable, head)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Foldable (foldr)
import Data.Rational (Rational)
import Data.Rational (fromInt, toNumber) as Q
import Data.Tuple (Tuple(..))

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

type Probability = Rational
type Singleton = {option :: Int, probability :: Probability}
type Couple = Tuple Singleton Singleton
data Conditional = Singleton Singleton | Couple Couple

instance showConditional :: Show Conditional where
  show (Singleton s) = "Singleton" <> show s
  show (Couple c) = "Couple" <> show c

predicateConditional :: (Probability -> Boolean) -> Conditional -> Boolean
predicateConditional f (Singleton s) = f s.probability
predicateConditional f (Couple (Tuple s1 s2)) =
  f $ s1.probability + s2.probability

defaultConditional :: Conditional -> Boolean
defaultConditional = predicateConditional (_ < Q.fromInt 1)

excessConditional :: Conditional -> Boolean
excessConditional = predicateConditional (_ > Q.fromInt 1)

completeConditionals :: Map Int Conditional -> Boolean
completeConditionals cs =
  (isEmpty $ Map.filter defaultConditional cs)
  && (isEmpty $ Map.filter excessConditional cs)

-- |
-- | Organize a discrete n-sized law of probability as an n-sized array
-- | of singletons or couples, more suited for drawing. For instance
-- |
-- |  A    B    C    D    =>       I          II             III       IV
-- | 0.2  0.3  0.1  0.4  abs:  (D:0.25) (D:0.15, C:0.1) (B:0.25) (B:0.05, A:0.2)
-- |                     rel:  (D:1)   (D:3/5, C:2/5)    (B:1)   (B:1/5, A:4/5)
-- |
-- | so that P(A) = P(A|IV)P(IV) = (4/5)(1/4) = 1/5
-- |         P(B) = P(B|III)P(III)+P(B|IV)P(IV) = (1)(1/4)+(1/5)(1/4) = 3/10
-- |         P(C) = P(C|II)P(II) = (2/5)(1/4) = 1/10
-- |         P(D) = P(D|I)P(I)+P(D|II)P(II) = (1)(1/4)+(3/5)(1/4) = 2/5
-- |

reshpeDiscreteDistribution :: Array Probability -> Array Conditional
reshpeDiscreteDistribution ps =
  let n = length $ filter (_ /= Q.fromInt 0) ps
      initial =
        foldr (\(Tuple i p) m ->
          if p > Q.fromInt 0
            then insert i
                   (Singleton { option: i
                              , probability: p * Q.fromInt n})
                   m
            else m) empty
            $ mapWithIndex (\i p -> Tuple i p) ps
      go cur =
        if completeConditionals cur
          then cur
          else
            let kexcess = unsafePartial $ fromJust $ head
                          $ keys $ Map.filter excessConditional cur
                kdefault =  unsafePartial $ fromJust $ head
                          $ keys $ Map.filter defaultConditional cur
                cexcess = case lookup kexcess cur of
                  Just (Singleton s) -> Just s
                  _                  -> Nothing
                cdefault = case lookup kdefault cur of
                  Just (Singleton s) -> Just s
                  _                  -> Nothing
                next = maybe empty identity
                  $ (\e d ->
                      let diff = Q.fromInt 1 - d.probability
                      in insert kexcess (Singleton { option: e.option
                                                   , probability: e.probability - diff
                                                   })
                        $ insert kdefault (Couple
                          $ Tuple d
                                  { option: e.option
                                  , probability: diff
                                  }) cur) <$> cexcess <*> cdefault
            in go next
  in toUnfoldable $ values $ go initial

draw :: Array Conditional -> Int -> Int -> Int
draw distribution r0 r1 =
  case distribution !! (r0 `mod` length distribution) of
            Singleton {option, probability} -> option
            Couple (Tuple {option: o1, probability: p1} {option: o2, probability}) ->
              let x = toNumber r1 / 10000.0
                in if x < Q.toNumber p1
                    then o1
                    else o2
