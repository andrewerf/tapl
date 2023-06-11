{-# LANGUAGE LambdaCase #-}

module Lambda2.Core.Typing
  (
  typeof,
  substType,
  kindOf
  )
where

import Lambda2.Core.AST
import Lambda2.Core.Errors
import Lambda2.Core.BasicTypes ( getBasicType )
import Control.Applicative (liftA2)


-- TODO:
--  uniform processing of terms and types. Use typeclass for something like "evaluatable" and
--  implement functor-like operations (e.g. substitution).


substType :: Type -> Int -> Type -> Type -- performs substitution [j -> s]t, where signature is t -> j -> s -> result
substType var@( TpVar k ) j s
  | k == j = s
  | otherwise = var

substType ( TpArrow tp1 tp2 ) j s = TpArrow ( substType tp1 j s ) ( substType tp2 j s )

substType ( TpPoly typeVarName knd tailType ) j s = TpPoly typeVarName knd ( substType tailType ( j + 1 ) ( shiftType0 1 s ) )

substType ( TpAbs typeVarName knd tailType ) j s = TpAbs typeVarName knd ( substType tailType ( j + 1 ) ( shiftType0 1 s ) )

substType ( TpApp tp1 tp2 ) j s = TpApp ( substType tp1 j s ) ( substType tp2 j s )


isTypeVal :: Type -> Bool
isTypeVal ( TpApp _ _ ) = False
isTypeVal _ = True

evalTypeApp1 :: Context -> Type -> Maybe Type
evalTypeApp1 _ ( TpApp ( TpAbs _ _ tpTail ) tp ) = Just $ shiftType0 ( -1 ) ( substType tpTail 0 ( shiftType0 1 tp ) )
evalTypeApp1 ctx ( TpApp t1 t2 )
  | not $ isTypeVal t1 = ( \t1' -> TpApp t1' t2 ) <$> evalTypeApp1 ctx t1
  | not $ isTypeVal t2 = TpApp t1 <$> evalTypeApp1 ctx t2
evalTypeApp1 ctx ( TpApp ( TpVar x ) tp ) = case getVar ctx x of
  Just ( _, realTp ) -> evalTypeApp1 ctx $ TpApp realTp tp
  Nothing -> Nothing
evalTypeApp1 _ _ = Nothing

evalTypeApp :: Context -> Type -> Maybe Type
evalTypeApp ctx tp 
  | isTypeVal tp = Just tp
  | otherwise = evalTypeApp1 ctx tp >>= evalTypeApp ctx

isCompatible :: Context -> Type -> Type -> Bool
isCompatible _ ( TpVar x ) ( TpVar y ) = x == y
isCompatible ctx ( TpArrow tp1 tp2 ) ( TpArrow tp1' tp2' )
  = maybe False ( uncurry (isCompatible ctx) ) (liftA2 (,) (evalTypeApp ctx tp1) (evalTypeApp ctx tp1')) &&
    maybe False ( uncurry (isCompatible ctx) ) (liftA2 (,) (evalTypeApp ctx tp2) (evalTypeApp ctx tp2'))
isCompatible ctx ( TpPoly s knd tp ) ( TpPoly _ _ tp' ) = isCompatible ( extendContextWithTypeVar s (TpVar 0) knd ctx )  tp tp'
isCompatible ctx app1 app2
  = maybe False ( uncurry (isCompatible ctx) ) (liftA2 (,) (evalTypeApp ctx app1) (evalTypeApp ctx app2))


kindOf :: Type -> Maybe Kind
kindOf ( TpVar _ ) = Just KndStar
kindOf ( TpArrow _ _ ) = Just KndStar
kindOf ( TpPoly _ _ _ ) = Just KndStar
kindOf ( TpAbs _ knd tp ) = KndArrow knd <$> kindOf tp
kindOf ( TpApp ( TpAbs _ _ tp1 ) _ ) = kindOf tp1
kindOf _ = Nothing


typeof :: Context -> Term -> Either ( ErrorM () ) Type
typeof ctx tm@( TmVar ( BoundVar i ) ) = case getVar ctx i of
  Nothing -> Left $ makeTypeError NoVarInContext ctx [tm] []
  Just ( _, varType ) -> Right varType

typeof ctx ( TmVar ( DataVar d ) ) = getBasicType ctx d

typeof ctx ( TmAbs varName varType ( TailAbs tailTerm ) ) = case typeof ( extendContextWithVar varName varType ctx ) tailTerm of
  Left err -> Left $ err >> makeTypeError BadTailType ctx [tailTerm] [varType]
  Right tailType -> Right $ TpArrow varType ( shiftType0 ( -1 ) tailType ) -- shift because the context was extended with 1 var

typeof _ ( TmAbs _ varType ( ActiveAbs _ tp _ ) ) = Right $ TpArrow varType tp

typeof ctx ( TmApp ( TmPoly typeVarName knd tailTerm ) ( TmType tp ) ) = case typeof ( extendContextWithTypeVar typeVarName tp knd ctx ) tailTerm of
  Left err -> Left $ err >> makeTypeError BadTailType ctx [tailTerm] []
  Right tailType -> Right $ shiftType0 ( -1 ) $ substType tailType 0 ( shiftType0 1 tp )

typeof ctx ( TmApp tm1 tm2 ) = case typeof ctx tm1 of
  Left err -> Left $ err >> errorConstructor BadLeftType []
  Right tp1 -> case typeof ctx tm2 of
    Left err -> Left $ err >> errorConstructor BadRightType []
    Right tp2 -> case tp1 of
      TpArrow tpArg tpRes -> if isCompatible ctx tpArg tp2
        then Right tpRes
        else Left $ errorConstructor TypesNotEq [tp1, tp2]
      TpPoly _ knd tpTail -> case tm2 of
        TmType tp3 -> case ( == knd ) <$> ( evalTypeApp ctx tp3 >>= kindOf ) of
          Just True -> Right $ shiftType0 ( -1 ) $ substType tpTail 0 ( shiftType0 1 tp3 )
          _ -> Left $ errorConstructor TypesNotEq [tp1, tp2]
        _ -> Left $ errorConstructor BadRightType [tp1, tp2]
      tpApp -> case evalTypeApp ctx tpApp of
        Just tp -> case tp of
          TpArrow tpArg tpRes -> if isCompatible ctx tpArg tp2
            then Right tpRes
            else Left $ errorConstructor TypesNotEq [tp1, tp2]
          TpPoly _ _ tpTail -> case tm2 of
            TmType tp3 -> Right $ shiftType0 ( -1 ) $ substType tpTail 0 ( shiftType0 1 tp3 )
            _ -> Left $ errorConstructor BadRightType [tp1, tp2]
          _ -> Left $ errorConstructor CouldNotEvalType [tp]
        _ -> Left $ errorConstructor CouldNotEvalType [tpApp]
--        Just ( TpArrow tpArg tpRes ) -> if isCompatible tpArg tp2
--          then Right tpRes
--          else Left $ errorConstructor TypesNotEq [tp1, tp2]
--        Just ( TpPoly _ knd tpTail ) -> case tm2 of
--          TmType tp3 -> case ( == knd ) <$> kindOf tp3 of
--            Just True -> Right $ shiftType0 ( -1 ) $ substType tpTail 0 ( shiftType0 1 tp3 )
--            _ -> Left $ errorConstructor TypesNotEq [tp1, tp2]
--          _ -> Left $ errorConstructor BadRightType [tp1, tp2]
--        Just tp -> Left $ errorConstructor CouldNotEvalType [tp]
--        _ -> Left $ errorConstructor CouldNotEvalType [tp1, tp2]
  where
    errorConstructor k = makeTypeError k ctx [tm1, tm2]

typeof _ ( TmType tp ) = Right tp

typeof ctx ( TmPoly typeVarName knd tailTerm ) = case typeof ( extendContextWithTypeVar typeVarName ( TpVar 0 ) knd ctx ) tailTerm of
  Left err -> Left $ err >> makeTypeError BadTailType ctx [tailTerm] []
  Right tailType -> Right $ TpPoly typeVarName knd tailType
