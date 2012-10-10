{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wall #-}
module ConstMath.Types (
  Opts(..)
, Verbosity(..)
, defaultOpts
, setVerbosity
, quiet
, verbose
, traced
) where

data Verbosity = None | Basic | CmVerbose Int | Trace deriving (Eq, Show, Ord)

data Opts = Opts
    { cmVerbosity :: Verbosity }
    deriving (Eq, Show)

setVerbosity :: Verbosity -> Opts -> Opts
setVerbosity cmVerbosity opts = Opts{cmVerbosity}

defaultOpts :: Opts
defaultOpts = Opts Basic

quiet   :: Opts -> Bool
quiet Opts{cmVerbosity} = cmVerbosity == None

verbose :: Opts -> Bool
verbose Opts{cmVerbosity} = cmVerbosity > Basic

traced :: Opts -> Bool
traced Opts{cmVerbosity} = cmVerbosity >= Trace
