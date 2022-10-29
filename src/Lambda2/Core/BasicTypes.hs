{-# LANGUAGE LambdaCase #-}

module Lambda2.Core.BasicTypes
  (
  getBasicType,
  basicContext,
  createBasicFunc
  )
where


import Lambda2.Core.AST
import Lambda2.Core.PreprocessType ( desugarType )
import Lambda2.Core.Errors


getBasicType :: Context -> TermData -> Either ( ErrorM () ) Type
getBasicType ctx = \case
  TdInt _ -> desugarType ctx ( TpsVar "Int" )
  TdFloat _ -> desugarType ctx ( TpsVar "Flaot" )
  TdString _ -> desugarType ctx ( TpsVar "String" )


makeBinding1 :: String -> ( TermData -> TermData ) -> Type -> Type -> Term
makeBinding1 bindingName f varType absType = TmAbs "x" varType $ ActiveAbs ( \case
  TmVar ( DataVar d ) -> TmVar $ DataVar $ f d
  _ -> error "This should never happen"
  ) absType ( Just bindingName )

makeBinding2 :: String -> ( TermData -> TermData -> TermData ) -> Type -> Type -> Type -> Term
makeBinding2 bindingName f varType1 varType2 absType = TmAbs "x" varType1 $ ActiveAbs ( \case
  TmVar ( DataVar d ) -> makeBinding1 ( bindingName ++ "'" ) ( f d ) varType2 absType
  _ -> error "This should never happen"
  ) ( TpArrow varType2 absType ) ( Just bindingName )


makePolyBinding1 :: String -> ( TermData -> TermData ) -> ( String -> Type ) -> Term
makePolyBinding1 bindingName f makeAbsType = TmPoly "t" $ makeBinding1 bindingName f ( TpVar 0 ) ( makeAbsType "t" )

makePolyBinding2 :: String -> ( TermData -> TermData -> TermData ) -> ( String -> Type ) -> Term
makePolyBinding2 bindingName f makeAbsType = TmPoly "t" $ makeBinding2 bindingName f ( TpVar 0 ) ( TpVar 0 ) ( makeAbsType "t" )


basicContext :: Context
basicContext = 
  extendContextWithVar "minus" ( TpPoly "t" ( TpArrow ( TpVar 0 ) ( TpVar 0 ) ) ) $
  extendContextWithVar "plus" ( TpPoly "t" ( TpArrow ( TpVar 0 ) ( TpVar 0 ) ) ) $
  extendContextWithTypeVar "Int" $
  extendContextWithTypeVar "Float" mempty


plus :: TermData -> TermData -> TermData
plus ( TdInt i ) ( TdInt j ) = TdInt ( i + j )
plus ( TdFloat i ) ( TdFloat j ) = TdFloat ( i + j )
plus ( TdString s1 ) ( TdString s2 ) = TdString ( s1 ++ s2 )
plus _ _ = error "This should never happen"

minus :: TermData -> TermData -> TermData
minus ( TdInt i ) ( TdInt j ) = TdInt ( i - j )
minus ( TdFloat i ) ( TdFloat j ) = TdFloat ( i - j )
minus ( TdString _ ) ( TdString _ ) = TdString ""
minus _ _ = error "This should never happen"

  
createBasicFunc :: String -> Maybe Term
createBasicFunc "plus" = Just $ makePolyBinding2 "plus" plus ( const ( TpVar 0 ) )
createBasicFunc "minus" = Just $ makePolyBinding2 "minus" minus ( const ( TpVar 0 ) )
createBasicFunc _ = Nothing

