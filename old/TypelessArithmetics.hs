module TypelessArithmetics where

import TypelessArithmeticsAST

isnum :: Term -> Bool
isnum TmZero = True
isnum ( TmSucc x ) = isnum x
isnum ( TmPred x ) = isnum x
isnum _ = False

isval :: Term -> Bool
isval TmTrue = True
isval TmFalse = True
isval term | isnum term = True
isval _ = False

eval1 :: Term -> Maybe Term
eval1 ( TmIf TmTrue x _ ) = Just x
eval1 ( TmIf TmFalse _ y ) = Just y
eval1 ( TmIf c x y ) = ( \c' -> TmIf c' x y ) <$> eval1 c
eval1 ( TmSucc x ) = TmSucc <$> eval1 x -- <$> is a synonym for `fmap`
eval1 ( TmPred TmZero ) = Nothing
eval1 ( TmPred ( TmSucc x ) )
  | isnum x = Just x
  | otherwise = Nothing
eval1 ( TmPred x ) = TmPred <$> eval1 x
eval1 ( TmIsZero TmZero ) = Just TmTrue
eval1 ( TmIsZero ( TmSucc _ ) ) = Just TmFalse
eval1 ( TmIsZero x ) = TmIsZero <$> eval1 x
eval1 _ = Nothing

eval :: Term -> Maybe Term
eval x
 | isval x = Just x
 | otherwise = eval1 x >>= eval
