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

data Verbosity = None | CmVerbose Int | Trace deriving (Eq, Show, Ord)

data Opts = Opts
    { cmVerbosity :: Verbosity }
    deriving (Eq, Show)

setVerbosity :: Verbosity -> Opts -> Opts
setVerbosity cmVerbosity opts = opts{cmVerbosity}

defaultOpts :: Opts
defaultOpts = Opts None

quiet   :: Opts -> Bool
quiet Opts{cmVerbosity} = cmVerbosity == None

verbose :: Opts -> Bool
verbose Opts{cmVerbosity} = cmVerbosity > CmVerbose 0

traced :: Opts -> Bool
traced Opts{cmVerbosity} = cmVerbosity >= Trace
