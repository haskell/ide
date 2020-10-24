{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Ide.Plugin.Tactic.GHC where

import Data.Maybe (isJust)
import Development.IDE.GHC.Compat
import OccName
import TcType
import TyCoRep
import Type
import TysWiredIn (intTyCon, floatTyCon, doubleTyCon, charTyCon)
import Unique
import Var


tcTyVar_maybe :: Type -> Maybe Var
tcTyVar_maybe ty | Just ty' <- tcView ty = tcTyVar_maybe ty'
tcTyVar_maybe (CastTy ty _) = tcTyVar_maybe ty  -- look through casts, as
                                                -- this is only used for
                                                -- e.g., FlexibleContexts
tcTyVar_maybe (TyVarTy v)   = Just v
tcTyVar_maybe _             = Nothing


instantiateType :: Type -> ([TyVar], Type)
instantiateType t = do
  let vs  = tyCoVarsOfTypeList t
      vs' = fmap cloneTyVar vs
      subst = foldr (\(v,t) a -> extendTCvSubst a v $ TyVarTy t) emptyTCvSubst
            $ zip vs vs'
   in (vs', substTy subst t)


cloneTyVar :: TyVar -> TyVar
cloneTyVar t =
  let uniq = getUnique t
      some_magic_number = 49
   in setVarUnique t $ deriveUnique uniq some_magic_number


------------------------------------------------------------------------------
-- | Is this a function type?
isFunction :: Type -> Bool
isFunction (tacticsSplitFunTy -> (_, _, [], _)) = False
isFunction _ = True


tacticsSplitFunTy :: Type -> ([TyVar], ThetaType, [Type], Type)
tacticsSplitFunTy t
  = let (vars, theta, t') = tcSplitSigmaTy t
        (args, res) = tcSplitFunTys t'
     in (vars, theta, args, res)

#if __GLASGOW_HASKELL__ < 810
mkVisFunTys :: [Type] -> Type -> Type
mkVisFunTys = mkFunTys
#endif


------------------------------------------------------------------------------
-- | Is this an algebraic type?
algebraicTyCon :: Type -> Maybe TyCon
algebraicTyCon (splitTyConApp_maybe -> Just (tycon, _))
  | tycon == intTyCon    = Nothing
  | tycon == floatTyCon  = Nothing
  | tycon == doubleTyCon = Nothing
  | tycon == charTyCon   = Nothing
  | tycon == funTyCon    = Nothing
  | otherwise = Just tycon
algebraicTyCon _ = Nothing

------------------------------------------------------------------------------
-- | Can ths type be lambda-cased?
--
-- Return: 'Nothing' if no
--         @Just False@ if it can't be homomorphic
--         @Just True@ if it can
lambdaCaseable :: Type -> Maybe Bool
lambdaCaseable (splitFunTy_maybe -> Just (arg, res))
  | isJust (algebraicTyCon arg)
  = Just $ isJust $ algebraicTyCon res
lambdaCaseable _ = Nothing

fromPatCompat :: PatCompat GhcTc -> Pat GhcTc
#if __GLASGOW_HASKELL__ == 808
type PatCompat pass = Pat pass
fromPatCompat = id
#else
type PatCompat pass = LPat pass
fromPatCompat = unLoc
#endif

------------------------------------------------------------------------------
-- | Should make sure it's a fun bind
pattern TopLevelRHS :: OccName -> [PatCompat GhcTc] -> LHsExpr GhcTc -> Match GhcTc (LHsExpr GhcTc)
pattern TopLevelRHS name ps body <-
  Match _
    (FunRhs (L _ (occName -> name)) _ _)
    ps
    (GRHSs _
      [L _ (GRHS _ [] body)] _)

getPatName :: PatCompat GhcTc -> Maybe OccName
getPatName (fromPatCompat -> p0) =
  case p0 of
    VarPat  _ x   -> Just $ occName $ unLoc x
    LazyPat _ p   -> getPatName p
    AsPat   _ x _ -> Just $ occName $ unLoc x
    ParPat  _ p   -> getPatName p
    BangPat _ p   -> getPatName p
    ViewPat _ _ p -> getPatName p
#if __GLASGOW_HASKELL__ >= 808
    SigPat  _ p _ -> getPatName p
#endif
#if __GLASGOW_HASKELL__ == 808
    XPat   p      -> getPatName $ unLoc p
#endif
    _             -> Nothing

