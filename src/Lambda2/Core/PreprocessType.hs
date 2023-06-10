module Lambda2.Core.PreprocessType
  (
  desugarType,
  desugarKind
  )
where


import Lambda2.Core.AST
import Lambda2.Core.Errors


desugarKind :: KindSimple -> Kind
desugarKind KndsStar = KndStar
desugarKind ( KndsArrow k1 k2 ) = KndArrow ( desugarKind k1 ) ( desugarKind k2 )


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

desugarType ctx ( TpsPoly varName knds tailTypeSimple ) = case desugarType ( extendContextWithTypeVar varName ( TpVar 0 ) knd ctx ) tailTypeSimple of
  Left err -> Left $ err >> makeSimpleDesugarError BadTailType ctx [] [tailTypeSimple]
  Right tailType -> Right $ TpPoly varName knd tailType
  where
    knd = desugarKind knds

desugarType ctx ( TpsAbs varName knds tailTypeSimple ) =  case desugarType ( extendContextWithTypeVar varName ( TpVar 0 ) knd ctx ) tailTypeSimple of
  Left err -> Left $ err >> makeSimpleDesugarError BadTailType ctx [] [tailTypeSimple]
  Right tailType -> Right $ TpAbs varName knd tailType
  where
    knd = desugarKind knds

desugarType ctx ( TpsApp t1s t2s ) = case desugarType ctx t1s of
  Left err -> Left $ err >> makeSimpleDesugarError BadHeadType ctx [] [t2s]
  Right t1 -> case desugarType ctx t2s of
    Left err -> Left $ err >> makeSimpleDesugarError BadTailType ctx [] [t1s]
    Right t2 -> Right $ TpApp t1 t2
