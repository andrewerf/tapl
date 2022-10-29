module Lambda2.Core.PreprocessType
  (
  desugarType
  )
where


import Lambda2.Core.AST
import Lambda2.Core.Errors


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

