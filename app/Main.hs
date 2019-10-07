module Main where

import           Clash.Prelude
import           Clash.Explicit.Testbench

import qualified Data.Text.IO                  as T

import           Fifo

{-# ANN topEntity (Synthesize
    { t_name = "fifo"
    , t_inputs =
        [ PortName "clk" , PortName "rst" , PortName "en"
        , PortProduct "" [PortName "din", PortName "ordy"]
        ]
    , t_output = PortProduct "" [PortName "irdy", PortName "dout"]
    }) #-}
{-# ANN topEntity (TestBench 'testBench) #-}
topEntity
    :: Clock System
    -> Reset System
    -> Enable System
    -> Signal System (Maybe (Unsigned 8), Bool)
    -> Signal System (Bool, Maybe (Unsigned 8))
topEntity = exposeClockResetEnable (fifo d2)
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    clk                     = tbSystemClockGen (not <$> done)
    rst                     = systemResetGen
    en                      = enableGen

    (inputData, outputData) = unzip testData

    stimuli                 = stimuliGenerator clk rst inputData
    verifier                = outputVerifier' clk rst outputData

    done                    = verifier $ topEntity clk rst en stimuli

testData :: Vec 10 ((Maybe (Unsigned 8), Bool), (Bool, Maybe (Unsigned 8)))
testData =
    ((Just 0, False), (True, Nothing))
        :> ((Just 1, False) , (True, Just 0))
        :> ((Just 2, False) , (True, Just 0))
        :> ((Just 3, False) , (True, Just 0))
        :> ((Just 4, False) , (False, Just 0))
        :> ((Nothing, True) , (False, Just 0))
        :> ((Nothing, True) , (True, Just 1))
        :> ((Nothing, True) , (True, Just 2))
        :> ((Nothing, True) , (True, Just 3))
        :> ((Nothing, False), (True, Nothing))
        :> Nil

main :: IO ()
main = do
    vcd <- dumpVCD (0, 10) testBench ["inputData"]

    case vcd of
        Left  msg -> error msg
        Right cnt -> T.writeFile "dump.vcd" cnt
