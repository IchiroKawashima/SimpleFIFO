{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ViewPatterns #-}

module Fifo
    ( fifo
    )
where

import           Clash.Prelude
import           Data.Maybe                     ( isJust )
import           Control.Monad                  ( guard )

import           Utils

import qualified Data.List                     as L
import           Debug.Trace

fifo
    :: forall dom addressSize a
     . (HiddenClockResetEnable dom, NFDataX a, ShowX a, 1 <= addressSize)
    => SNat addressSize
    -> Signal dom (Maybe a, Bool)
    -> Signal dom (Bool, Maybe a)
fifo addressSize@SNat (unbundle -> (inputData, outputReady)) = trace tr
    $ bundle (inputReady, outputData)
  where
    tr = makeTable
        10
        [ ("put"    , ShownSignal put)
        , ("get"    , ShownSignal get)
        , ("n_waddr", ShownSignal nextWriteAddress)
        , ("n_raddr", ShownSignal nextReadAddress)
        , ("waddr"  , ShownSignal writeAddress)
        , ("raddr"  , ShownSignal nextWriteAddress)
        , ("full"   , ShownSignal isFull)
        , ("empty"  , ShownSignal isEmpty)
        ]

    put = (isJust <$> inputData) .&&. (not <$> isFull)
    get = outputReady .&&. (not <$> isEmpty)

    nextWriteAddress :: Signal dom (Unsigned (1 + addressSize))
    nextWriteAddress = mux put (succ <$> writeAddress) writeAddress
    nextReadAddress :: Signal dom (Unsigned (1 + addressSize))
    nextReadAddress = mux get (succ <$> readAddress) readAddress

    writeAddress    = register 0 nextWriteAddress
    readAddress     = register 0 nextReadAddress

    isFull          = register False $ nextWriteAddress .==. nextReadAddress'
      where
        nextReadAddress' = do
            b  <- slice addressSize addressSize <$> nextReadAddress
            bs <- slice (addressSize `subSNat` d1) d0 <$> nextReadAddress

            return $ unpack $ complement b ++# bs

    isEmpty    = register True $ nextWriteAddress .==. nextReadAddress

    inputReady = not <$> isFull

    ramInput   = do
        full  <- isFull
        wdata <- inputData
        waddr <- unpack . slice (SNat @(addressSize - 1)) d0 <$> writeAddress

        return $ guard (not full) >> (,) <$> pure waddr <*> wdata

    ramOutput = readNew (blockRamPow2 (repeat undefined)) raddr ramInput
      where
        raddr = unpack . slice (SNat @(addressSize - 1)) d0 <$> nextReadAddress

    outputData = do
        empty <- isEmpty
        rdata <- ramOutput

        return $ guard (not empty) >> pure rdata
