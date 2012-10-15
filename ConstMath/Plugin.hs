{-# LANGUAGE ViewPatterns #-}

module ConstMath.Plugin (
      plugin -- :: Plugin
) where

import ConstMath.Types
import ConstMath.Pass (constMathProgram)
import GhcPlugins

plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install args todos = do
    reinitializeGlobals
    return $ insertPasses opts todos
  where opts = parseOpts args

-- TODO: use a real parser
parseOpts :: [CommandLineOption] -> Opts
parseOpts = foldr ($) defaultOpts . map mkArg
    where
        mkArg flag
            | flag `elem` ["-v","--verbose","--verbosity=1"]    = setVerbosity (CmVerbose 1)
            | flag `elem` ["-v11", "-verbosity=11","--trace"] = setVerbosity Trace
            | flag `elem` ["-q", "--quiet","--verbosity=0", "-v0"]  = setVerbosity None
            | flag `elem` ["--dry", "--dry-run"]  = setDry
            | flag `elem` ["--enable-always"]     = setInsertion CmAlways
            | flag `elem` ["--enable-default"]    = setInsertion CmPostSimplifyEarly
            | flag `elem` ["--enable-post-simpl"] = setInsertion CmPostSimplify
            | otherwise = id

----------------------------------------------------------------
-- Phase control
--

insertPasses :: Opts -> [CoreToDo] -> [CoreToDo]
insertPasses opts todos = foldr genPass [] $ zip todos [0..]
    where
        constMath n = CoreDoPluginPass ("Constant Math Elimination - " ++ show n)
                        (bindsOnlyPass (constMathProgram n opts) )
        genPass p@(todo,n) rest
          | matchPass opts p = todo : constMath n : rest
          | otherwise        = todo : rest

-- In most cases, new replacements are only visible to this plugin after a
-- simplifier pass, although there are often some from the beginning.
-- Therefore, the standard strategy is to insert a ConstMath pass after a
-- simplifier pass
--
-- we also always insert a pass after the initial phase, because a lot of
-- expressions are visible then too.
matchPass :: Opts -> (CoreToDo, Int) -> Bool
matchPass _ (_,0)                     = True
matchPass (cmInsertion -> CmAlways) _ = True
matchPass (cmInsertion -> CmPostSimplify) (todo,_)
    = hasSimplifierPass todo
matchPass (cmInsertion -> CmPostSimplifyEarly) (todo,n)
    = hasSimplifierPass todo && n <= 10
matchPass _ _ = False

hasSimplifierPass :: CoreToDo -> Bool
hasSimplifierPass (CoreDoSimplify _ _) = True
hasSimplifierPass (CoreDoPasses todos) = any hasSimplifierPass todos
hasSimplifierPass _                    = False
