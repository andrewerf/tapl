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

evalTypeApp :: Context -> Type -> Maybe Type
evalTypeApp _ ( TpApp ( TpAbs _ _ tpTail ) tp ) = Just $ substType tpTail 0 tp
evalTypeApp ctx ( TpApp ( TpVar x ) tp ) = case getVar ctx x of
  Just ( _, realTp ) -> evalTypeApp ctx $ TpApp realTp tp
  Nothing -> Nothing
evalTypeApp _ _ = Nothing


isCompatible :: Context -> Type -> Type -> Bool
isCompatible ctx ( TpPoly _ knd1 t1 ) ( TpPoly _ knd2 t2 ) = isCompatible ctx t1 t2 && knd1 == knd2
isCompatible ctx ( TpArrow tpArg1 tpRes1 ) ( TpArrow tpArg2 tpRes2 ) = isCompatible ctx tpArg1 tpArg2 && isCompatible ctx tpRes1 tpRes2
isCompatible ctx app@( TpApp _ _ ) t = app == t || case evalTypeApp ctx app of
  Just tp -> isCompatible ctx tp t
  Nothing -> False
isCompatible ctx t1 t2 = t1 == t2


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
        TmType tp3 -> case ( == knd ) <$> kindOf tp3 of
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
