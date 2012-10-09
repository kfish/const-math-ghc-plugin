
module ConstMath.Pass (
      constMathProgram
) where

import GhcPlugins

constMathProgram :: [CoreBind] -> CoreM [CoreBind]
constMathProgram binds = return binds
