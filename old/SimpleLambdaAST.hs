{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
module SimpleLambdaAST where

import Data.Maybe

data TypeSimple =
  TpsVar String |
  TpsArrow TypeSimple TypeSimple |
  TpsPoly String TypeSimple
  deriving ( Eq )

data TermSimple =
  TmsVar String |
  TmsAbs String TypeSimple TermSimple |
  TmsApp TermSimple TermSimple |
  TmsLet String TermSimple TermSimple |
  TmsPoly String TermSimple
  deriving ( Eq, Show )

data Type =
  TpVar Int |
  TpArrow Type Type |
  TpPoly String Type
  deriving ( Eq )

data Term =
  TmVar Int |
  TmAbs String Type Term |
  TmApp Term Term |
  TmPoly String Term |
  TmType Type
  deriving ( Eq, Show )


type NamingContext = [String]
type TypingContext = [Type]
data ContextMember = VarWithType String Type | TypeWithStar String
type Context = [ContextMember]
data TermWithContext = TermWithContext Context Term
data TypeWithContext = TypeWithContext Context Type


instance Show TypeWithContext where
  show ( TypeWithContext ctx tp ) = case tp of
    TpVar t -> show $ ctx !! t
    TpArrow t1 t2 -> "(" ++ show ( TypeWithContext ctx t1 ) ++ "->" ++ show ( TypeWithContext ctx t2 ) ++ ")"
    TpPoly typeVarName tail -> "@" ++ typeVarName' ++ ":*." ++ show ( TypeWithContext ctx' tail )
      where ( ctx', typeVarName' ) = newTypeName ctx typeVarName
    where
      newTypeName :: Context -> String -> ( Context, String )
      newTypeName ctx typeVarName
        | typeVarName `notElem` getPolyTypes ctx = ( TypeWithStar typeVarName : ctx, typeVarName )
        | otherwise = newTypeName ctx ( typeVarName ++ "'" )


--instance Show ContextMember where
--  show ( VarWithType varName tp ) = varName ++ " : " ++ show tp
--  show ( TypeWithStar t ) = show t

instance Show TermWithContext where
  show ( TermWithContext context term ) =
    case term of
      TmAbs varName varType tailTerm -> "(\\" ++ varName' ++ ":" ++ show ( TypeWithContext context varType ) ++ "." ++ show ( TermWithContext context' tailTerm ) ++ ")"
        where
          ( context', varName' ) = newVarWithType context varName varType
      TmApp term1 term2 -> "(" ++ show ( TermWithContext context term1 ) ++ " " ++ show ( TermWithContext context term2 ) ++ ")"
      TmVar index -> getNamingContext context !! index
      TmPoly typeVarName tailTerm -> "(\\" ++ typeVarName ++ ":*." ++ show ( TermWithContext ( TypeWithStar typeVarName : context ) tailTerm )
      TmType tp -> show ( TypeWithContext context tp )
    where
      newVarWithType :: Context -> String -> Type -> ( Context, String )
      newVarWithType context varName tp
        | varName `notElem` getNamingContext context = ( VarWithType varName tp : context, varName )
        | otherwise = newVarWithType context ( varName ++ "'" ) tp
