module Main where

import System.Cmd
import System.Directory

import Paths_const_math_ghc_plugin

main = do
    testdir <- getDataFileName "tests"
    setCurrentDirectory testdir
    system "make && make clean"
