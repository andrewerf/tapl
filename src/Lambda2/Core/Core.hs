module Lambda2.Core.Core
  (
  eval
  )
where

import Lambda2.Core.AST
import Lambda2.Core.Typing

isval :: Term -> Bool
isval ( TmAbs {} ) = True
isval ( TmVar {} ) = True
isval ( TmPoly {} ) = True
isval ( TmType {} ) = True
isval _ = False


subst :: Term -> Int -> Term -> Term -- performs substitution [j -> s]t, where signature is t -> j -> s -> TermResult
subst var@( TmVar k ) j s
  | k == j = s
  | otherwise = var
subst ( TmAbs varName varType tailTerm ) j s = TmAbs varName varType ( subst tailTerm ( j + 1 ) ( shift0 1 s ) )
subst ( TmApp t1 t2 ) j s = TmApp ( subst t1 j s ) ( subst t2 j s )
subst tm@( TmType _ ) _ _ = tm
subst ( TmPoly typeVarName tailTerm ) j s = TmPoly typeVarName ( subst tailTerm ( j + 1 ) ( shift0 1 s ) )

substTypeInTerm :: Term -> Int -> Type -> Term
substTypeInTerm var@( TmVar _ ) _ _ = var
substTypeInTerm ( TmAbs varName varType tailTerm ) j s = TmAbs varName ( substType varType j s ) ( substTypeInTerm tailTerm ( j + 1 ) ( shiftType0 1 s ) )
substTypeInTerm ( TmApp t1 t2 ) j s = TmApp ( substTypeInTerm t1 j s ) ( substTypeInTerm t2 j s )
substTypeInTerm ( TmType tp ) j s = TmType ( substType tp j s )
substTypeInTerm ( TmPoly typeVarName tailTerm ) j s = TmPoly typeVarName ( substTypeInTerm tailTerm ( j + 1 ) ( shiftType0 1 s ) )


eval1 :: Term -> Either String Term
eval1 ( TmApp ( TmAbs _ _ tailTerm ) term )
  | isval term = Right $ shift0 ( -1 ) ( subst tailTerm 0 ( shift0 1 term ) )

eval1 ( TmApp ( TmPoly _ tailTerm ) ( TmType tp ) )  = Right $ shift0 ( -1 ) ( substTypeInTerm tailTerm 0 ( shiftType0 1 tp  ) )

eval1 ( TmApp term1 term2 )
  | not $ isval term1 = ( \term1' -> TmApp term1' term2 ) <$> eval1 term1
  | not $ isval term2 = TmApp term1 <$> eval1 term2

eval1 _ = Left "No rule to apply"

eval :: Term -> Either String Term
eval x
 | isval x = Right x
 | otherwise = eval1 x >>= eval
