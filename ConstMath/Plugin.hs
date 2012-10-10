module ConstMath.Plugin (
      plugin -- :: Plugin
) where

import ConstMath.Types
import ConstMath.Pass (constMathProgram)
import GhcPlugins

import Data.List (intersperse)

plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install args todos = do
    reinitializeGlobals
    let pass = CoreDoPasses [constMath]
    return $ intersperse pass todos
  where constMath = CoreDoPluginPass "Constant Math Elimination"
            (bindsOnlyPass (constMathProgram (parseOpts args)) )

-- TODO: use a real parser
parseOpts :: [CommandLineOption] -> Opts
parseOpts = foldr ($) defaultOpts . map mkArg
    where
        mkArg flag
            | flag `elem` ["-v","--verbose","--verbosity=1"]    = setVerbosity Basic
            | flag `elem` ["-v11", "-verbosity=11","--trace"] = setVerbosity Trace
            | flag `elem` ["-q", "--quiet","--verbosity=0", "-v0"]  = setVerbosity None
            | otherwise = id
