{-# LANGUAGE ExistentialQuantification #-}

module Utils where

import           Clash.Prelude

import           Text.Layout.Table
import qualified Data.List                     as L

data ShownSignal dom
    = forall a. (ShowX a, NFDataX a) => ShownSignal (Signal dom a)

showSignal :: (KnownDomain dom) => Int -> ShownSignal dom -> [String]
showSignal numberOfCycles (ShownSignal signal) =
    L.tail $ showX <$> sampleN (numberOfCycles + 1) signal

makeTable :: (KnownDomain dom) => Int -> [(String, ShownSignal dom)] -> String
makeTable numberOfcycles signals = tableString aligns asciiS headers rows
  where
    aligns  = fixedLeftCol 10 : L.repeat (column expand right def def)
    headers = titlesH $ "Signal" : [ "#" L.++ show n | n <- [1 ..] ]
    rows =
        [ rowG (name : showSignal numberOfcycles wave)
        | (name, wave) <- signals
        ]
