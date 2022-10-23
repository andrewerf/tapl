module Lambda2.Core.Preprocess
  (
  desugarType,
  desugar
  )
where

import Lambda2.Core.AST
import Lambda2.Core.Errors
import Lambda2.Core.Typing



desugarType :: Context -> TypeSimple ->  Either ( ErrorM () ) Type
desugarType ctx tp@( TpsVar varName ) = case getVarIndex ctx varName of
  Just i -> Right $ TpVar i
  Nothing -> Left $ makeSimpleDesugarError NoVarInContext ctx [] [tp]

desugarType ctx ( TpsArrow tp1simple tp2simple ) = case desugarType ctx tp1simple of
  Left err -> Left $ err >> errorConstructor BadLeftType
  Right tp1 -> case desugarType ctx tp2simple of
    Left err -> Left $ err >> errorConstructor BadRightType
    Right tp2 -> Right $ TpArrow tp1 tp2
  where
    errorConstructor k = makeSimpleDesugarError k ctx [] [tp1simple, tp2simple]

desugarType ctx ( TpsPoly varName tailTypeSimple ) = case desugarType ( extendContextWithTypeVar varName ctx ) tailTypeSimple of
  Left err -> Left $ err >> makeSimpleDesugarError BadTailType ctx [] [tailTypeSimple]
  Right tailType -> Right $ TpPoly varName tailType


desugar :: Context -> TermSimple -> Either ( ErrorM () ) Term
desugar ctx tm@( TmsVar varName ) = case getVarIndex ctx varName of
  Just i -> Right $ TmVar i
  Nothing -> Left $ makeSimpleDesugarError NoVarInContext ctx [tm] []

desugar ctx ( TmsAbs varName varTypeSimple tailTermSimple ) = case desugarType ctx varTypeSimple of
  Left err -> Left $ err >> errorConstructor BadHeadType
  Right varType -> case desugar ( extendContextWithVar varName varType ctx ) tailTermSimple of
    Left err -> Left $ err >> errorConstructor BadTailTerm
    Right tailTerm -> Right $ TmAbs varName varType tailTerm
  where
    errorConstructor k = makeSimpleDesugarError k ctx [tailTermSimple] [varTypeSimple]

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
      Right t2 -> Right $ TmApp ( TmAbs varName tp1 t2 ) t1
  where
    errorConstructor k = makeSimpleDesugarError k ctx [t1simple, t2simple] []
