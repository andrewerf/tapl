module SimpleLambda where

import SimpleLambdaAST

isval :: Term -> Bool
isval ( TmAbs {} ) = True
isval ( TmVar _ ) = True
isval _ = False

shift :: Term -> Int -> Int -> Term -- TermToShift -> Shift -> Cutoff -> Term
shift ( TmVar k ) d c
  | k < c = TmVar k -- doesn't shift as variable is bounded
  | otherwise = TmVar ( k + d )
shift ( TmAbs varName varType tailTerm ) d c = TmAbs varName varType ( shift tailTerm d ( c + 1 ) )
shift ( TmApp t1 t2 ) d c = TmApp ( shift t1 d c ) ( shift t2 d c )

shift0 :: Term -> Int -> Term -- shifts with zero cutoff
shift0 t d = shift t d 0

subst :: Term -> Int -> Term -> Term -- performs substitution [j -> s]t, where signature is t -> j -> s -> TermResult
subst var@( TmVar k ) j s
  | k == j = s
  | otherwise = var
subst ( TmAbs varName varType tailTerm ) j s = TmAbs varName varType ( subst tailTerm ( j + 1 ) ( shift0 s 1 ) )
subst ( TmApp t1 t2 ) j s = TmApp ( subst t1 j s ) ( subst t2 j s )

eval1 :: Term -> TermEither
eval1 ( TmApp ( TmAbs _ _ tailTerm ) term )
  | isval term = Right $ shift0 ( subst tailTerm 0 ( shift0 term 1 ) ) ( -1 )
eval1 ( TmApp term1 term2 )
  | not $ isval term1 = ( \term1' -> TmApp term1' term2 ) <$> eval1 term1
  | not $ isval term2 = TmApp term1 <$> eval1 term2
eval1 term = Left $ TermError term "No rule to apply"

eval :: Term -> TermEither
eval x
 | isval x = Right x
 | otherwise = eval1 x >>= eval
