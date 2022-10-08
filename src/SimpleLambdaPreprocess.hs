{-# LANGUAGE DuplicateRecordFields #-}
module SimpleLambdaPreprocess where

import Data.List ( elemIndex )

import SimpleLambdaAST
import SimpleLambdaErrors
import SimpleLambdaTypes
import Control.Applicative (liftA2)


desugarAndRemoveNames :: Context -> TermSimple -> Either DesugarError Term
desugarAndRemoveNames ctx term@( TmsVar name ) = case elemIndex name ( getNamingContext ctx ) of
  Just i -> Right ( TmVar i )
  Nothing -> Left $ VarDesugarError{varName=name, context=ctx}

desugarAndRemoveNames ctx ( TmsAbs varName varType t ) = case desugarAndRemoveNames ( VarWithType varName varType : ctx ) t of
  Right term -> Right $ TmAbs varName varType term
  Left err -> Left $ AbsDesugarError{varName=varName, tailTerm=Left err}

desugarAndRemoveNames ctx term@( TmsApp t1 t2 ) = case ( desugarAndRemoveNames ctx t1, desugarAndRemoveNames ctx t2 ) of
  ( Right t1', Right t2' ) -> Right $ TmApp t1' t2'
  ( e1, e2 ) -> Left $ AppDesugarError{leftTermSimple = t1, rightTermSimple=t2, leftTerm=e1, rightTerm=e2}

desugarAndRemoveNames ctx term@( TmsLet varName t1 t2 ) =
  let
    err = LetDesugarError{varName=varName, headTermSimple=t1, tailTermSimple=t2}
    maybeDesugaredTerm1 = desugarAndRemoveNames ctx t1
    maybeType1 = case maybeDesugaredTerm1 of
      ( Left e ) -> Left e
      ( Right term ) -> case typeof ctx term of
        ( Left e ) -> Left $ err{headTerm=Right term, tailTerm=Left $ VarDesugarError{varName=varName, context=ctx}, headType=Left e}
        ( Right tp ) -> Right tp
    maybeDesugaredTerm2 = maybeType1 >>= \type1 -> desugarAndRemoveNames ( VarWithType varName type1 : ctx  ) t2
    maybeAbs = liftA2 ( TmAbs varName ) maybeType1 maybeDesugaredTerm2
  in
    liftA2 TmApp maybeAbs maybeDesugaredTerm1
