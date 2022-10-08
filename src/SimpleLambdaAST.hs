{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
module SimpleLambdaAST where

data Type =
  TypeVar String |
  TypeArrow Type Type
  deriving ( Eq )

data TermSimple =
  TmsVar String |
  TmsAbs String Type TermSimple |
  TmsApp TermSimple TermSimple |
  TmsLet String TermSimple TermSimple
  deriving ( Eq, Show )

data Term =
  TmVar Int |
  TmAbs String Type Term |
  TmApp Term Term
  deriving ( Eq, Show )


type NamingContext = [String]
type TypingContext = [Type]
data VarWithType = VarWithType String Type
type Context = [VarWithType]
data TermWithContext = TermWithContext Context Term

getNamingContext :: Context -> NamingContext
getNamingContext = map ( \( VarWithType n _ ) -> n )

getTypingContext :: Context -> TypingContext
getTypingContext = map ( \( VarWithType _ t ) -> t )

instance Show Type where
  show ( TypeVar t ) = t
  show ( TypeArrow t1 t2 ) = show t1 ++ "->" ++ show t2

instance Show VarWithType where
  show ( VarWithType varName tp ) = varName ++ " : " ++ show tp

instance Show TermWithContext where
  show ( TermWithContext context term ) =
    case term of
      TmAbs varName varType tailTerm -> "(\\" ++ varName' ++ ":" ++ show varType ++ "." ++ show ( TermWithContext context' tailTerm ) ++ ")"
        where
          ( context', varName' ) = newVarWithType context varName varType
      TmApp term1 term2 -> show ( TermWithContext context term1 ) ++ " " ++ show ( TermWithContext context term2 )
      TmVar index -> getNamingContext context !! index
    where
      newVarWithType :: Context -> String -> Type -> ( Context, String )
      newVarWithType context varName tp
        | varName `notElem` getNamingContext context = ( VarWithType varName tp : context, varName )
        | otherwise = newVarWithType context ( varName ++ "'" ) tp
