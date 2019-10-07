module FifoSpec
    ( spec
    )
where

import           Test.Hspec
import           Clash.Prelude
import           Clash.Prelude.Testbench
import qualified Data.List                     as L

import           Fifo

sim :: Int -> [(Maybe Integer, Bool)] -> [(Bool, Maybe Integer)]
sim n = simulateN n (fifo @System d2)

spec :: Spec
spec = describe "fifo" $ do
    it "can set flags properly" $ do
        let (stm, exp) = L.unzip
                [ ((Just 0, False) , (True, Nothing))
                , ((Just 1, False) , (True, Just 0))
                , ((Just 2, False) , (True, Just 0))
                , ((Just 3, False) , (True, Just 0))
                , ((Just 4, False) , (False, Just 0))
                , ((Nothing, True) , (False, Just 0))
                , ((Nothing, True) , (True, Just 1))
                , ((Nothing, True) , (True, Just 2))
                , ((Nothing, True) , (True, Just 3))
                , ((Nothing, False), (True, Nothing))
                ]
        sim 10 stm `shouldBe` exp

    it "can accept data continuously" $ do
        let (stm, exp) = L.unzip
                [ ((Just 0, True) , (True, Nothing))
                , ((Just 1, True) , (True, Just 0))
                , ((Just 2, True) , (True, Just 1))
                , ((Just 3, True) , (True, Just 2))
                , ((Just 4, True) , (True, Just 3))
                , ((Just 5, True) , (True, Just 4))
                , ((Just 6, True) , (True, Just 5))
                , ((Just 7, True) , (True, Just 6))
                , ((Nothing, True), (True, Just 7))
                , ((Nothing, True), (True, Nothing))
                ]
        sim 10 stm `shouldBe` exp
