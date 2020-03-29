module Test.Main where

import Prelude

import PRNG (PRNG, normals, prng, uniforms, exponentials, chunks, frequency, mean, stdev)
import Data.Array (concat)

import Effect (Effect)
import Effect.Class.Console (log)

sigma = 1.0 :: Number
mu = 0.0 :: Number
lambda = 0.25 :: Number

subSampleSize = 100 :: Int
sampleSize = 200 * subSampleSize :: Int

rnd = prng {val: 0, gen: 0, seed: 1468714885} :: PRNG

normalSample = concat $ normals 1 sampleSize mu sigma rnd :: Array Number

exponentialSample = concat $ exponentials 1 sampleSize lambda rnd :: Array Number

subExpSample = mean <$> chunks subSampleSize exponentialSample :: Array Number


main :: Effect Unit
main = do
  log $ show $ uniforms 2 3 2.0 6.0 rnd
  log $ show $ normals 3 2 mu sigma rnd
  log $ show $ frequency (\x -> -1.0 < x && x < 1.0) normalSample -- 0.6827
  log $ show $ frequency (\x -> -2.0 < x && x < 2.0) normalSample -- 0.9545
  log $ show $ frequency (\x -> -3.0 < x && x < 3.0) normalSample -- 0.9973
  log $ show $ mean exponentialSample -- 1 / lambda
  log $ show $ mean subExpSample      -- 1 / lambda
  log $ show $ stdev subExpSample     -- 1 / lambda / sqrt subSampleSize
 