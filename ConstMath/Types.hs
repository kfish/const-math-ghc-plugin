{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wall #-}
module ConstMath.Types (
-- *Types
-- ** Configuration Options
  Opts(..)
, Verbosity(..)
, defaultOpts
-- *Configuration Functions
-- ** Opt setters
, setVerbosity
, setDry
-- ** Opt checkers
, quiet
, verbose
, traced
, dry
) where

data Verbosity = None | CmVerbose Int | Trace deriving (Eq, Show, Ord)

data Opts = Opts
    { cmVerbosity :: Verbosity
    , dryRun      :: Bool
    }
    deriving (Eq, Show)

setVerbosity :: Verbosity -> Opts -> Opts
setVerbosity cmVerbosity opts = opts{cmVerbosity}

setDry :: Opts -> Opts
setDry opts = opts{dryRun=True}

defaultOpts :: Opts
defaultOpts = Opts None False

quiet   :: Opts -> Bool
quiet Opts{cmVerbosity} = cmVerbosity == None

verbose :: Opts -> Bool
verbose Opts{cmVerbosity} = cmVerbosity > CmVerbose 0

traced :: Opts -> Bool
traced Opts{cmVerbosity} = cmVerbosity >= Trace

dry :: Opts -> Bool
dry Opts{dryRun} = dryRun
