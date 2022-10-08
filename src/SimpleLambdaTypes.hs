{-# LANGUAGE DuplicateRecordFields #-}
module SimpleLambdaTypes where

import Relude.List

import SimpleLambdaAST
import SimpleLambdaErrors


typeof :: Context -> Term -> Either TypeError Type
typeof ctx ( TmVar i ) = case ctx !!? i of
  Just ( VarWithType _ t ) -> Right t
  Nothing -> Left VarTypeError{varIndex=Just i, context=ctx}

typeof ctx ( TmAbs varName varType tailTerm ) = case typeof ( VarWithType varName varType : ctx ) tailTerm of
  Right tailType -> Right $ TypeArrow varType tailType
  Left typeError -> Left AbsTypeError{varName=varName, tailTerm=typeError}

typeof ctx ( TmApp t1 t2 ) = case ( typeof ctx t1, typeof ctx t2 ) of
  ( Right leftType@( TypeArrow type1 type2 ), Right type1' ) -> if type1 == type1'
    then Right type2
    else Left AppTypeError{leftTerm=TermWithContext ctx t1, rightTerm=TermWithContext ctx t2, leftType=Right leftType, rightType=Right type1'}
  ( e1, e2 ) -> Left AppTypeError{leftTerm=TermWithContext ctx t1, rightTerm=TermWithContext ctx t2, leftType=e1, rightType=e2}
