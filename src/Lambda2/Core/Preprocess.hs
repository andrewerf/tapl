module Lambda2.Core.Preprocess
  (
  desugar
  )
where

import Lambda2.Core.AST
import Lambda2.Core.Errors
import Lambda2.Core.Typing
import Lambda2.Core.PreprocessType ( desugarType )
import Lambda2.Core.BasicTypes


desugar :: Context -> TermSimple -> Either ( ErrorM () ) Term
desugar ctx tm@( TmsVar ( BoundVar varName ) ) = case createBasicFunc varName of
  Just activeTm -> Right activeTm
  Nothing -> case getVarIndex ctx varName of
    Just i -> Right $ TmVar ( BoundVar i )
    Nothing -> Left $ makeSimpleDesugarError NoVarInContext ctx [tm] []

desugar _ ( TmsVar ( DataVar d ) ) = Right $ TmVar ( DataVar d )

desugar ctx ( TmsAbs varName varTypeSimple ( TailAbs tailTermSimple ) ) = case desugarType ctx varTypeSimple of
  Left err -> Left $ err >> errorConstructor BadHeadType
  Right varType -> case desugar ( extendContextWithVar varName varType ctx ) tailTermSimple of
    Left err -> Left $ err >> errorConstructor BadTailTerm
    Right tailTerm -> Right $ TmAbs varName varType $ TailAbs tailTerm
  where
    errorConstructor k = makeSimpleDesugarError k ctx [tailTermSimple] [varTypeSimple]

desugar ctx ( TmsAbs varName varTypeSimple ( ActiveAbs f tp n ) ) = case desugarType ctx varTypeSimple of
  Left err -> Left $ err >> errorConstructor BadHeadType
  Right varType -> TmAbs varName varType . ( \tp' -> ActiveAbs f tp' n ) <$> desugarType ctx tp
  where
    errorConstructor k = makeSimpleDesugarError k ctx [] [varTypeSimple]

desugar ctx ( TmsApp t1simple t2simple ) = case desugar ctx t1simple of
  Left err -> Left $ err >> errorConstructor BadLeftTerm
  Right t1 -> case desugar ctx t2simple of
    Left err -> Left $ err >> errorConstructor BadRightTerm
    Right t2 -> Right $ TmApp t1 t2
  where
    errorConstructor k = makeSimpleDesugarError k ctx [t1simple, t2simple] []

desugar ctx ( TmsType tpsimple ) = case desugarType ctx tpsimple of
  Left err -> Left $ err >> makeSimpleDesugarError BadHeadType ctx [] [tpsimple]
  Right tp -> Right $ TmType tp

desugar ctx ( TmsPoly typeVarName tailTermSimple ) = case desugar ( extendContextWithTypeVar typeVarName ctx ) tailTermSimple of
  Left err -> Left $ err >> makeSimpleDesugarError BadTailTerm ctx [TmsType (TpsVar typeVarName ), tailTermSimple] []
  Right tailTerm -> Right $ TmPoly typeVarName tailTerm

desugar ctx ( TmsLet varName t1simple t2simple ) = case desugar ctx t1simple of
  Left err -> Left $ err >> errorConstructor BadLeftTerm
  Right t1 -> case typeof ctx t1 of
    Left err -> Left $ err >> errorConstructor BadLeftTerm
    Right tp1 -> case desugar ( extendContextWithVar varName tp1 ctx ) t2simple of
      Left err -> Left $ err >> errorConstructor BadRightTerm
      Right t2 -> Right $ TmApp ( TmAbs varName tp1 ( TailAbs t2 ) ) t1
  where
    errorConstructor k = makeSimpleDesugarError k ctx [t1simple, t2simple] []
