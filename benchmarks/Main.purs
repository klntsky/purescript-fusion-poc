module BenchMain where

import Prelude

import Benchotron.Core (Benchmark, benchFn, mkBenchmark)
import Benchotron.UI.Console (runSuite)
import Control.Fusion (runFusion, (:<<<))
import Control.Fusion.Types as F
import Data.Array ((..))
import Data.Array as A
import Data.Foldable (foldr)
import Effect (Effect)
import Test.QuickCheck.Gen (stateful, Gen)

benchIdentity :: Benchmark
benchIdentity = mkBenchmark
  { slug: "identity"
  , title: "Make use of `map identity = identity`"
  , sizes: A.range 0 10 <#> (_ * 100)
  , sizeInterpretation: "n"
  , inputsPerSize: 3
  , gen
  , functions: [ benchFn "map identity"
                 (\n -> map identity $ map identity $ (0..n))
               , benchFn "map identity (with fusion)"
                 (\n -> runFusion (F.map F.identity) (0..n))
               , benchFn "map identity <<< map identity (with fusion)"
                 (\n -> runFusion (F.map F.identity :<<< F.map F.identity) (0..n))
               , benchFn "map (identity <<< identity) (with fusion)"
                 (\n -> runFusion (F.map (F.identity :<<< F.identity)) (0..n))
               ]
  }

benchMapFusion :: Benchmark
benchMapFusion = mkBenchmark
  { slug: "mapFusion"
  , title: "Make use of `map f <<< map g = map (f <<< g)`"
  , sizes: A.range 0 10 <#> (_ * 2000)
  , sizeInterpretation: "n"
  , inputsPerSize: 3
  , gen
  , functions: [ benchFn "map f <<< map f"
                 (\n -> map succ <<< map succ $ (0..n))
               , benchFn "map f <<< map f (with fusion)"
                 (\n -> runFusion (F.map succ :<<< F.map succ) (0..n))

               , benchFn "map f <<< map f <<< map f <<< map f"
                 (\n -> map succ <<< map succ <<< map succ <<< map succ $ (0..n))
               , benchFn "map f <<< map f <<< map f <<< map f (with fusion)"
                 (\n -> runFusion (F.map succ :<<< F.map succ :<<< F.map succ :<<< F.map succ) (0..n))
               ]
  }

benchFoldrMap :: Benchmark
benchFoldrMap = mkBenchmark
  { slug: "foldrMapFusion"
  , title: "Make use of `foldr f c <<< map g = foldr (f <<< g) c`"
  , sizes: A.range 0 10 <#> (_ * 400)
  , sizeInterpretation: "n"
  , inputsPerSize: 3
  , gen
  , functions: [ benchFn "foldr add zero <<< map succ"
                 (\n -> foldr add zero <<< map succ $ (0..n))
               , benchFn "foldr add zero <<< map succ (with fusion)"
                 -- sadly, type annotation is required
                 (\n -> runFusion (F.foldr (add :: Int -> _) 0 :<<< F.map succ) (0..n))
               ]
  }

main :: Effect Unit
main = do
  runSuite [ benchMapFusion
           , benchFoldrMap
           , benchIdentity
           ]

succ :: Int -> Int
succ x = x + 1

gen :: forall a. a -> Gen a
gen = \n -> stateful (\seed -> pure n)
