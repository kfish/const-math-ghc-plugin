module ConstMath.Plugin (
      plugin -- :: Plugin
) where

import ConstMath.Pass (constMathProgram)
import GhcPlugins

import Data.List (intersperse)

plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todos = do
    reinitializeGlobals
    let pass = CoreDoPasses [constMath]
    return $ intersperse pass todos
  where constMath = CoreDoPluginPass "Constant Math Elimination" (bindsOnlyPass constMathProgram)
