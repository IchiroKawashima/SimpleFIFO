#!/usr/bin/env stack
{- stack script
   --resolver lts-14.7
   --package shelly
   --package optparse-simple
   --package text
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import           Shelly
import           Options.Applicative.Simple
import           Control.Applicative
import           Data.Monoid
import qualified Data.Text                     as T
default (T.Text)

main :: IO ()
main = do
    ((), ()) <- simpleOptions "ver 1.0"
                              "Synth"
                              "Synthesise topEntity module."
                              (pure ())
                              empty

    shelly $ escaping False $ run_
        "stack exec -- clash"
        ["--verilog", "-isrc", "-outputdir build", "app/Main.hs"]
