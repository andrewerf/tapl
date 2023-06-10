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
subst var@( TmVar ( BoundVar k ) ) j s
  | k == j = s
  | otherwise = var
subst var@( TmVar _ ) _ _ = var
subst ( TmAbs varName varType ( TailAbs tailTerm ) ) j s = TmAbs varName varType $ TailAbs ( subst tailTerm ( j + 1 ) ( shift0 1 s ) )
subst t@( TmAbs {}  ) _ _ = t
subst ( TmApp t1 t2 ) j s = TmApp ( subst t1 j s ) ( subst t2 j s )
subst tm@( TmType _ ) _ _ = tm
subst ( TmPoly typeVarName knd tailTerm ) j s = TmPoly typeVarName knd ( subst tailTerm ( j + 1 ) ( shift0 1 s ) )


substTypeInTerm :: Term -> Int -> Type -> Term
substTypeInTerm var@( TmVar _ ) _ _ = var
substTypeInTerm ( TmAbs varName varType ( TailAbs tailTerm ) ) j s = TmAbs varName ( substType varType j s ) $ TailAbs ( substTypeInTerm tailTerm ( j + 1 ) ( shiftType0 1 s ) )
substTypeInTerm ( TmAbs varName varType activeAbs ) j s = TmAbs varName ( substType varType j s ) activeAbs
substTypeInTerm ( TmApp t1 t2 ) j s = TmApp ( substTypeInTerm t1 j s ) ( substTypeInTerm t2 j s )
substTypeInTerm ( TmType tp ) j s = TmType ( substType tp j s )
substTypeInTerm ( TmPoly typeVarName knd tailTerm ) j s = TmPoly typeVarName knd ( substTypeInTerm tailTerm ( j + 1 ) ( shiftType0 1 s ) )


eval1 :: Context -> Term -> Either String Term
eval1 _ ( TmApp ( TmAbs _ _ ( TailAbs tailTerm ) ) term )
  | isval term = Right $ shift0 ( -1 ) ( subst tailTerm 0 ( shift0 1 term ) )

eval1 _ ( TmApp ( TmAbs _ _ ( ActiveAbs f _ _ ) ) term )
  | isval term = Right $ f term

eval1 _ ( TmApp ( TmPoly _ _ tailTerm ) ( TmType tp ) )  = Right $ shift0 ( -1 ) ( substTypeInTerm tailTerm 0 ( shiftType0 1 tp  ) )

eval1 ctx ( TmApp term1 term2 )
  | not $ isval term1 = ( \term1' -> TmApp term1' term2 ) <$> eval1 ctx term1
  | not $ isval term2 = TmApp term1 <$> eval1 ctx term2

eval1 _ _ = Left "No rule to apply"


eval :: Context -> Term -> Either String Term
eval ctx x
 | isval x = Right x
 | otherwise = eval1 ctx x >>= eval ctx
