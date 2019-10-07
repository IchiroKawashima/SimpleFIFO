{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ViewPatterns #-}

module Fifo
    ( fifo
    )
where

import           Clash.Prelude
import           Data.Maybe                     ( isJust
                                                , fromMaybe
                                                )
import           Control.Monad                  ( guard )

fifo
    :: forall dom addressSize a
     . (HiddenClockResetEnable dom, NFDataX a, ShowX a, 1 <= addressSize)
    => SNat addressSize
    -> Signal dom (Maybe a, Bool)
    -> Signal dom (Bool, Maybe a)
fifo addressSize@SNat (unbundle -> (inputData, outputReady)) = tr
    $ bundle (inputReady, outputData)
  where
    tr =
        seq
            $     traceSignal "inputData"
            $     fromMaybe undefined
            <$>   inputData
            `seq` traceSignal "inputReady" inputReady
            `seq` traceSignal "outputData"
            $     fromMaybe undefined
            <$>   outputData
            `seq` traceSignal "outputReady" outputReady
            `seq` traceSignal "put"         put
            `seq` traceSignal "get"         get
            `seq` traceSignal "nxt_waddr"   nextWriteAddress
            `seq` traceSignal "nxt_raddr"   nextReadAddress
            `seq` traceSignal "raddr"       writeAddress
            `seq` traceSignal "waddr"       readAddress
            `seq` traceSignal "full"        isFull
            `seq` traceSignal "empty"       isEmpty

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
