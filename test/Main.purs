module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Control.Fusion as F
import Control.Fusion ((:<<<), runFusion)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Effect.Class (liftEffect)

main :: Effect Unit
main = do
  runTest do
    suite "fusion" do
      test "map" do
        Assert.assert "map succ"
          (runFusion (F.map succ) [1,2,3] == [2,3,4])
        Assert.assert "map succ <<< map succ"
          (runFusion (F.map succ :<<< F.map succ) [1,2,3] == [3,4,5])
        Assert.assert "foldr add 0"
          (runFusion (F.foldr (add :: Int -> _) 0) [1,2,3] == 6)
        Assert.assert "foldr add 0 <<< map succ"
          (runFusion (F.foldr (add :: Int -> _) 0 :<<< F.map succ) [1,2,3] == 9)

succ :: Int -> Int
succ x = x + 1
