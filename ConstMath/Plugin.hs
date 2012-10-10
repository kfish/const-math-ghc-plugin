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

-- You should probably run this with -fno-cse !
install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todos = do
    reinitializeGlobals
    let pass = CoreDoPasses [constMath]
    return $ intersperse pass todos
  where constMath = CoreDoPluginPass "Constant Math Elimination" (bindsOnlyPass constMathProgram)
