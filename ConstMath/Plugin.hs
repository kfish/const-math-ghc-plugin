module ConstMath.Plugin (
      plugin -- :: Plugin
) where

import ConstMath.Pass (constMathProgram)
import GhcPlugins

plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install
  }

-- You should probably run this with -fno-cse !
install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todos = do
    reinitializeGlobals
    return $ CoreDoPasses [constMath] : todos
  where constMath = CoreDoPluginPass "Constant Math Elimination" (bindsOnlyPass constMathProgram)
