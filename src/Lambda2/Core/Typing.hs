module Lambda2.Core.Typing
  (
  typeof,
  substType
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

substType ( TpPoly typeVarName tailType ) j s = TpPoly typeVarName ( substType tailType ( j + 1 ) ( shiftType0 1 s ) )


typeof :: Context -> Term -> Either ( ErrorM () ) Type
typeof ctx tm@( TmVar ( BoundVar i ) ) = case getVar ctx i of
  Nothing -> Left $ makeTypeError NoVarInContext ctx [tm] []
  Just ( _, varType ) -> Right varType

typeof ctx ( TmVar ( DataVar d ) ) = getBasicType ctx d

typeof ctx ( TmAbs varName varType ( TailAbs tailTerm ) ) = case typeof ( extendContextWithVar varName varType ctx ) tailTerm of
  Left err -> Left $ err >> makeTypeError BadTailType ctx [tailTerm] [varType]
  Right tailType -> Right $ TpArrow varType ( shiftType0 ( -1 ) tailType ) -- shift because the context was extended with 1 var
  
typeof _ ( TmAbs _ varType ( ActiveAbs _ tp _ ) ) = Right $ TpArrow varType tp 

typeof ctx ( TmApp tm1 tm2 ) = case typeof ctx tm1 of
  Left err -> Left $ err >> errorConstructor BadLeftType
  Right tp1 -> case typeof ctx tm2 of
    Left err -> Left $ err >> errorConstructor BadRightType
    Right tp2 -> case tp1 of
      TpArrow tpArg tpRes -> if tpArg == tp2
        then Right tpRes
        else Left $ errorConstructor TypesNotEq
      TpPoly _ tpTail -> case tm2 of
        TmType tp3 -> Right $ substType tpTail 0 tp3
        _ -> Left $ errorConstructor BadRightType
      _ -> Left $ errorConstructor BadLeftType
  where
    errorConstructor k = makeTypeError k ctx [tm1, tm2] []

typeof _ ( TmType tp ) = Right tp

typeof ctx ( TmPoly typeVarName tailTerm ) = case typeof ( extendContextWithTypeVar typeVarName ctx ) tailTerm of
  Left err -> Left $ err >> makeTypeError BadTailType ctx [tailTerm] []
  Right tailType -> Right $ TpPoly typeVarName tailType
