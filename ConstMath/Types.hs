{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wall #-}
module ConstMath.Types (
-- *Types
-- ** Configuration Options
  Opts(..)
, Verbosity(..)
, CmInsertion(..)
, defaultOpts
-- *Configuration Functions
-- ** Opt setters
, setVerbosity
, setDry
, setInsertion
-- ** Opt checkers
, quiet
, verbose
, traced
, dry
) where

data Verbosity = None | CmVerbose Int | Trace deriving (Eq, Show, Ord)

-- | Controls where the ConstMath pass is performed.
data CmInsertion =
      CmPostSimplifyEarly        -- ^ initially and after simplifier passes (within first 10 passes)
    | CmPostSimplify             -- ^ initially and after every simplifier pass
    | CmAlways                   -- ^ after every pass (expensive and usually wasteful)
    deriving (Eq, Show, Ord)

data Opts = Opts
    { cmVerbosity :: Verbosity
    , dryRun      :: Bool
    , cmInsertion :: CmInsertion
    }
    deriving (Eq, Show)

setVerbosity :: Verbosity -> Opts -> Opts
setVerbosity cmVerbosity opts = opts{cmVerbosity}

setDry :: Opts -> Opts
setDry opts = opts{dryRun=True}

setInsertion :: CmInsertion -> Opts -> Opts
setInsertion cmInsertion opts = opts{cmInsertion}

defaultOpts :: Opts
defaultOpts = Opts None False CmPostSimplifyEarly

quiet   :: Opts -> Bool
quiet Opts{cmVerbosity} = cmVerbosity == None

verbose :: Opts -> Bool
verbose Opts{cmVerbosity} = cmVerbosity > CmVerbose 0

traced :: Opts -> Bool
traced Opts{cmVerbosity} = cmVerbosity >= Trace

dry :: Opts -> Bool
dry Opts{dryRun} = dryRun
