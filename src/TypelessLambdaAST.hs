{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
module TypelessLambdaAST where

import Data.List ( elemIndex )
import Data.Either ( lefts )



data TermSimple =
  TmsVar String |
  TmsAbs String TermSimple |
  TmsApp TermSimple TermSimple |
  TmsLet String TermSimple TermSimple
  deriving ( Eq, Show )

data Term =
  TmVar Int |
  TmAbs String Term |
  TmApp Term Term
  deriving ( Eq, Show )

type Context = [String]

data TermWithContext = TermWithContext Context Term


data TermError = forall a.(Show a) => TermError {
  errTerm :: a,
  errMsg :: String
}

instance Show TermError where
  show ( TermError errTerm errMsg ) = "Error: " ++ errMsg ++ "\n\t on term: " ++ show errTerm

type TermEither = Either TermError Term


desugar :: TermSimple -> TermSimple
desugar ( TmsLet s t1 t2 ) = TmsApp ( TmsAbs s ( desugar t2 ) ) ( desugar t1 )
desugar ( TmsAbs s t ) = TmsAbs s ( desugar t )
desugar ( TmsApp t1 t2 ) = TmsApp ( desugar t1 ) ( desugar t2 )
desugar ( TmsVar s ) = TmsVar s

removeNames :: Context -> TermSimple -> TermEither
removeNames ctx term@( TmsVar name ) = case elemIndex name ctx of
  Just i -> Right ( TmVar i )
  Nothing -> Left $ TermError term $ "No " ++ name ++ " in context: " ++ foldl1 ( \s1 s2 -> s1 ++ ", " ++ s2 ) ctx
removeNames ctx ( TmsAbs varName t ) = TmAbs varName <$> removeNames ( varName : ctx ) t
removeNames ctx term@( TmsApp t1 t2 ) = case ( removeNames ctx t1, removeNames ctx t2 ) of
  ( Right t1', Right t2' ) -> Right $ TmApp t1' t2'
  ( e1, e2 ) -> Left $ TermError term $ foldl1 (++) $ map ( \( TermError _ e ) -> e ) $ lefts [e1, e2]
removeNames _ term = Left $ TermError term "Conversion to de Bron term is called on sugared term somehow"

newVarName :: Context -> String -> ( Context, String )
newVarName context varName
  | varName `notElem` context = ( varName : context, varName )
  | otherwise = newVarName context ( varName ++ "'" )


instance Show TermWithContext where
  show ( TermWithContext context term ) =
    case term of
      TmAbs varName tailTerm -> "(\\" ++ varName' ++ "." ++ show ( TermWithContext context' tailTerm ) ++ ")"
        where ( context', varName' ) = newVarName context varName
      TmApp term1 term2 -> show ( TermWithContext context term1 ) ++ " " ++ show ( TermWithContext context term2 )
      TmVar index -> context !! index
