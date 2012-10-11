{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wall #-}
module ConstMath.Pass (
      constMathProgram
) where

import ConstMath.Types

import Control.Applicative ((<$>))
import Control.Monad ((<=<))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import GhcPlugins

constMathProgram :: Opts -> [CoreBind] -> CoreM [CoreBind]
constMathProgram opts binds = do
    traceMsg opts "\nStarting ConstMath pass"
    mapM (subBind opts "") binds

subBind :: Opts -> String -> CoreBind -> CoreM CoreBind
subBind opts tab (NonRec b rhs) = do
    traceMsg opts $ tab ++ "Non-recursive binding named " ++ pretty b
    rhs' <- subExpr opts tab rhs
    return (NonRec b rhs')
subBind opts _tab bndr@(Rec pairs) = do
    _ <- mapM (uncurry $ printRecBind opts) pairs
    return bndr

printRecBind opts b _e = do
    traceMsg opts $ "Recursive binding " ++ pretty b

subExpr :: Opts -> String -> CoreExpr -> CoreM CoreExpr

subExpr opts tab expr@(Type t) = do
    traceMsg opts $ tab ++ "Type " ++ pretty t
    return expr

subExpr opts tab expr@(Coercion _co) = do
    traceMsg opts $ tab ++ "Coercion"
    return expr

subExpr opts tab expr@(Lit lit) = do
    traceMsg opts $ tab ++ "Lit " ++ pretty lit
    return expr

subExpr opts tab expr@(Var v) = do
    traceMsg opts $ tab ++ "Var " ++ pretty v
    return expr

subExpr opts tab (App f a) = do
    traceMsg opts $ tab ++ "App " ++ pretty f
    f' <- subExpr opts (tab ++ "< ") f
    a' <- subExpr opts (tab ++ "> ") a
    collapse opts (App f' a')

subExpr opts tab (Tick t e) = do
    traceMsg opts $ tab ++ "Tick"
    e' <- subExpr opts (tab ++ "  ") e
    return (Tick t e')

subExpr opts tab (Cast e co) = do
    traceMsg opts $ tab ++ "Cast"
    e' <- subExpr opts (tab ++ "  ") e
    return (Cast e' co)

subExpr opts tab (Lam b e) = do
    traceMsg opts $ tab ++ "Lam"
    e' <- subExpr opts (tab ++ "  ") e
    return (Lam b e')

subExpr opts tab (Let bind e) = do
    traceMsg opts $ tab ++ "Let"
    bind' <- subBind opts tab bind
    e' <- subExpr opts (tab ++ "  ") e
    return (Let bind' e')

subExpr opts tab (Case scrut bndr ty alts) = do
    traceMsg opts $ tab ++ "Case"
    let subAlt (ac,bs,eB) = (ac,bs,) <$> subExpr opts (tab ++ "  ") eB
    scrut' <- subExpr opts (tab ++ "  ") scrut
    alts' <- mapM subAlt alts
    return (Case scrut' bndr ty alts')

----------------------------------------------------------------------

collapse :: Opts -> CoreExpr -> CoreM CoreExpr
collapse opts expr@(App f1 _)
  | Just f <- cmSubst <$> findSub f1
    = f opts expr
collapse _ expr = return expr

mkUnaryCollapseIEEE :: (forall a. RealFloat a => (a -> a))
                    -> Opts
                    -> CoreExpr
                    -> CoreM CoreExpr
mkUnaryCollapseIEEE fnE opts expr@(App f1 (App f2 (Lit lit)))
    | isDHash f2, MachDouble d <- lit = e d mkDoubleLitDouble
    | isFHash f2, MachFloat d  <- lit = e d mkFloatLitFloat
    where
        e d = evalUnaryIEEE opts fnE f1 f2 d expr
mkUnaryCollapseIEEE _ _ expr = return expr

evalUnaryIEEE :: (Fractional a, RealFloat b)
              => Opts
              -> (a -> b)
              -> CoreExpr
              -> CoreExpr
              -> Rational
              -> CoreExpr
              -> (b -> Arg Var)
              -> CoreM (CoreExpr)
evalUnaryIEEE opts fnE f1 f2 d expr mkLit = do
    let sub = fnE (fromRational d)
    maybe (return expr)
      (return . App f2 . mkLit)
      =<< maybeIEEE opts (fromJust $ funcName f1) sub

mkUnaryCollapseNum :: (forall a . Num a => (a -> a))
                   -> Opts
                   -> CoreExpr
                   -> CoreM CoreExpr
mkUnaryCollapseNum fnE opts expr@(App f1 (App f2 (Lit lit)))
    | isDHash f2, MachDouble d <- lit =
        evalUnaryIEEE opts fnE f1 f2 d expr mkDoubleLitDouble
    | isFHash f2, MachFloat d  <- lit =
        evalUnaryIEEE opts fnE f1 f2 d expr mkFloatLitFloat
    | isIHash f2, MachInt d    <- lit =
        evalUnaryNum fromIntegral d mkIntLitInt
    | isWHash f2, MachWord d   <- lit =
        evalUnaryNum fromIntegral d mkWordLitWord
    where
        evalUnaryNum from d mkLit = do
            let sub = fnE (from d)
            return (App f2 (mkLit sub))
mkUnaryCollapseNum _ _ expr = return expr

mkBinaryCollapse :: (forall a. RealFloat a => (a -> a -> a))
                 -> Opts
                 -> CoreExpr
                 -> CoreM CoreExpr
mkBinaryCollapse fnE opts expr@(App (App f1 (App f2 (Lit lit1))) (App f3 (Lit lit2)))
    | isDHash f2 && isDHash f3
    , MachDouble d1 <- lit1, MachDouble d2 <- lit2 =
        evalBinaryIEEE d1 d2 mkDoubleLitDouble
    | isFHash f2 && isFHash f3
    , MachFloat d1  <- lit1, MachFloat d2  <- lit2 =
        evalBinaryIEEE d1 d2 mkFloatLitFloat
    where
        evalBinaryIEEE d1 d2 mkLit = do
            let sub = fnE (fromRational d1) (fromRational d2)
            maybe (return expr) (\x -> return (App f2 (mkLit x)))
                  =<< maybeIEEE opts (fromJust $ funcName f1) sub
mkBinaryCollapse _ _ expr = return expr

fromRationalCollapse :: Opts -> CoreExpr -> CoreM CoreExpr
fromRationalCollapse opts expr@(App f1 (App (App f2 (Lit (LitInteger n _))) (Lit (LitInteger d _))))
    | Just "ConstMath.Rules.rationalToFloat" <- funcName f1
    , Just "GHC.Real.:%" <- funcName f2
      = do
          let sub = fromRational $ (fromInteger n) / (fromInteger d)
          maybe (return expr) (\x -> return (mkFloatExpr x)) =<< maybeIEEE opts (fromJust $ funcName f1) sub
fromRationalCollapse opts expr@(App f1 (App (App f2 (Lit (LitInteger n _))) (Lit (LitInteger d _))))
    | Just "ConstMath.Rules.rationalToDouble" <- funcName f1
    , Just "GHC.Real.:%" <- funcName f2
      = do
          let sub = fromRational $ (fromInteger n) / (fromInteger d)
          maybe (return expr) (\x -> return (mkDoubleExpr x)) =<< maybeIEEE opts (fromJust $ funcName f1) sub
fromRationalCollapse _ expr = return expr

maybeIEEE :: RealFloat a => Opts -> String -> a -> CoreM (Maybe a)
maybeIEEE opts s d
    | isNaN d = do
        err "NaN"
        return Nothing
    | isInfinite d = do
        err "infinite"
        return Nothing
    | isDenormalized d = do
        err "denormalized"
        return Nothing
    | isNegativeZero d = do
        err "negative zero"
        return Nothing
    | otherwise = do
        msg opts $ "Result of replacing " ++ s ++ " is ok"
        return (Just d)
    where
        err v = errorMsgS $ "Skipping replacement of " ++ s ++ " result " ++ v

----------------------------------------------------------------------

data CMSub = CMSub
    { cmFuncName :: String
    , cmSubst    :: Opts -> CoreExpr -> CoreM CoreExpr
    }

unarySubIEEE :: String -> (forall a. RealFloat a => a -> a) -> CMSub
unarySubIEEE nm fn = CMSub nm (mkUnaryCollapseIEEE fn)

unarySubNum :: String -> (forall a . Num a => (a -> a)) -> CMSub
unarySubNum nm fn = CMSub nm (mkUnaryCollapseNum fn)

binarySub :: String -> (forall a. RealFloat a => a -> a -> a) -> CMSub
binarySub nm fn = CMSub nm (mkBinaryCollapse fn)

funcName :: CoreExpr -> Maybe String
funcName = listToMaybe . words . pretty

isFHash :: CoreExpr -> Bool
isFHash = maybe False ((==) "GHC.Types.F#") . funcName

isDHash :: CoreExpr -> Bool
isDHash = maybe False ((==) "GHC.Types.D#") . funcName

isIHash :: CoreExpr -> Bool
isIHash = maybe False ((==) "GHC.Types.I#") . funcName

isWHash :: CoreExpr -> Bool
isWHash = maybe False ((==) "GHC.Word.W#") . funcName

findSub :: CoreExpr -> Maybe CMSub
findSub = flip Map.lookup subFunc <=< funcName

subs :: [CMSub]
subs =
    [ unarySubIEEE "GHC.Float.exp"    exp
    , unarySubIEEE "GHC.Float.log"    log
    , unarySubIEEE "GHC.Float.sqrt"   sqrt
    , unarySubIEEE "GHC.Float.sin"    sin
    , unarySubIEEE "GHC.Float.cos"    cos
    , unarySubIEEE "GHC.Float.tan"    tan
    , unarySubIEEE "GHC.Float.asin"   asin
    , unarySubIEEE "GHC.Float.acos"   acos
    , unarySubIEEE "GHC.Float.atan"   atan
    , unarySubIEEE "GHC.Float.sinh"   sinh
    , unarySubIEEE "GHC.Float.cosh"   cosh
    , unarySubIEEE "GHC.Float.tanh"   tanh
    , unarySubIEEE "GHC.Float.asinh"  asinh
    , unarySubIEEE "GHC.Float.acosh"  acosh
    , unarySubIEEE "GHC.Float.atanh"  atanh
    , unarySubNum "GHC.Num.negate"   negate
    , unarySubNum "GHC.Num.abs"      abs
    , unarySubNum "GHC.Num.signum"   signum
    , CMSub    "ConstMath.Rules.rationalToFloat" fromRationalCollapse
    , CMSub    "ConstMath.Rules.rationalToDouble" fromRationalCollapse
    ]

subFunc :: Map String CMSub
subFunc = Map.fromList $ zip (map cmFuncName subs) subs

----------------------------------------------------------------------

msg :: Opts -> String -> CoreM ()
msg opts s
    | not (quiet opts) = putMsgS s
    | otherwise = return ()

vMsg :: Opts -> String -> CoreM ()
vMsg opts s
    | verbose opts = putMsgS s
    | otherwise    = return ()

traceMsg :: Opts -> String -> CoreM ()
traceMsg opts s
    | traced opts = putMsgS s
    | otherwise   = return ()

----------------------------------------------------------------------

pretty :: Outputable a => a -> String
pretty = showSDoc . ppr
