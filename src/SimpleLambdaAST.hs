{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
module SimpleLambdaAST where

import Relude.List
import Data.List ( elemIndex )
import Data.Either ( lefts )
import Control.Applicative ( liftA2 )


data Type =
  TypeVar String |
  TypeArrow Type Type
  deriving ( Eq, Show )

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

data TermError = forall a.Show a => TermError {
  errTerm :: a,
  errMsg :: String
}

type TermEither = Either TermError Term
type TypeEither = Either TermError Type

class TermWithContext a where
  type Context a :: *
  getTerm :: a -> Term
  getContext :: a -> Context a

type NamingContext = [String]
data TermWithNamingContext = TermWithNamingContext NamingContext Term

instance TermWithContext TermWithNamingContext where
  type Context TermWithNamingContext = NamingContext
  getTerm ( TermWithNamingContext _ term ) = term
  getContext ( TermWithNamingContext ctx _ ) = ctx

type TypingContext = [Type]
data TermWithTypingContext = TermWithTypingContext TypingContext Term

instance TermWithContext TermWithTypingContext where
  type Context TermWithTypingContext = TypingContext
  getTerm ( TermWithTypingContext _ term ) = term
  getContext ( TermWithTypingContext ctx _ ) = ctx

instance Show TermError where
  show ( TermError errTerm errMsg ) = "Error: " ++ errMsg ++ "\n\t on term: " ++ show errTerm


typeof :: TypingContext -> Term -> TypeEither
typeof ctx term@( TmVar i ) = case ctx !!? i of
  Just t -> Right t
  Nothing -> Left $ TermError term $ "No var with index: " ++ show i ++ " in context: " ++ show ctx
typeof ctx ( TmAbs _ varType tailTerm ) = TypeArrow varType <$> typeof ( varType : ctx ) tailTerm
typeof ctx term@( TmApp t1 t2 ) = case ( typeof ctx t1, typeof ctx t2 ) of
  ( Right leftType@( TypeArrow type1 type2 ), Right type1' ) -> if type1 == type1'
    then Right type2
    else Left $ TermError term $ "Bad application t1 t2 with types: " ++ show leftType ++ ", " ++ show type1' ++ "\n\t\tinput types are not equal."
  ( Right leftType, Right rightType ) -> Left $ TermError term $ "Bad application t1 t2 with types: " ++ show leftType ++ ", " ++ show rightType ++ "\n\t\tleft type is not arrow."
  _ -> Left $ TermError term "Bad application t1 t2"


desugarAndRemoveNames :: [(String,Type)] -> TermSimple -> TermEither
desugarAndRemoveNames ctx term@( TmsVar name ) = case elemIndex name namingCtx of
  Just i -> Right ( TmVar i )
  Nothing -> Left $ TermError term $ "No " ++ name ++ " in context: " ++ foldl1 ( \s1 s2 -> s1 ++ ", " ++ s2 ) namingCtx
  where
    namingCtx = map fst ctx

desugarAndRemoveNames ctx ( TmsAbs varName varType t ) = TmAbs varName varType <$> desugarAndRemoveNames ( ( varName, varType ) : ctx ) t

desugarAndRemoveNames ctx term@( TmsApp t1 t2 ) = case ( desugarAndRemoveNames ctx t1, desugarAndRemoveNames ctx t2 ) of
  ( Right t1', Right t2' ) -> Right $ TmApp t1' t2'
  ( e1, e2 ) -> Left $ TermError term $ foldl1 (++) $ map ( \( TermError _ e ) -> e ) $ lefts [e1, e2]

desugarAndRemoveNames ctx term@( TmsLet varName t1 t2 ) =
  let
    maybeDesugaredTerm1 = desugarAndRemoveNames ctx t1
    maybeType1 = maybeDesugaredTerm1 >>= typeof ( map snd ctx )
    maybeDesugaredTerm2 = maybeType1 >>= \type1 -> desugarAndRemoveNames ( ( varName, type1 ) : ctx  ) t2
    maybeAbs = liftA2 ( TmAbs varName ) maybeType1 maybeDesugaredTerm2
  in
    liftA2 TmApp maybeAbs maybeDesugaredTerm1

newVarName :: NamingContext -> String -> ( NamingContext, String )
newVarName context varName
  | varName `notElem` context = ( varName : context, varName )
  | otherwise = newVarName context ( varName ++ "'" )


instance Show TermWithNamingContext where
  show ( TermWithNamingContext context term ) =
    case term of
      TmAbs varName varType tailTerm -> "(\\" ++ varName' ++ ":" ++ show varType ++ "." ++ show ( TermWithNamingContext context' tailTerm ) ++ ")"
        where ( context', varName' ) = newVarName context varName
      TmApp term1 term2 -> show ( TermWithNamingContext context term1 ) ++ " " ++ show ( TermWithNamingContext context term2 )
      TmVar index -> context !! index
