module TypelessLambdaAST where

import Data.List ( elemIndex )



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

desugar :: TermSimple -> TermSimple
desugar ( TmsLet s t1 t2 ) = TmsApp ( TmsAbs s ( desugar t2 ) ) ( desugar t1 )
desugar ( TmsAbs s t ) = TmsAbs s ( desugar t )
desugar ( TmsApp t1 t2 ) = TmsApp ( desugar t1 ) ( desugar t2 )
desugar ( TmsVar s ) = TmsVar s

extractTerm :: TermWithContext -> Term
extractTerm ( TermWithContext _ term ) = term

removeNames :: Context -> TermSimple -> Maybe Term
removeNames ctx ( TmsVar name ) = TmVar <$> elemIndex name ctx
removeNames ctx ( TmsAbs varName t ) = TmAbs varName <$> removeNames ( varName : ctx ) t
removeNames ctx ( TmsApp t1 t2 ) = case ( removeNames ctx t1, removeNames ctx t2 ) of
  ( Just t1', Just t2' ) -> Just $ TmApp t1' t2'
  _ -> Nothing

newVarName :: Context -> String -> ( Context, String )
newVarName context varName
  | varName `notElem` context = ( varName : context, varName )
  | otherwise = newVarName context ( varName ++ "'" )

instance Show TermWithContext where
  show ( TermWithContext context term ) =
    case term of
      TmAbs varName tailTerm -> "\\" ++ varName' ++ "." ++ show ( TermWithContext context' tailTerm )
        where ( context', varName' ) = newVarName context varName
      TmApp term1 term2 -> show ( TermWithContext context term1 ) ++ " " ++ show ( TermWithContext context term2 )
      TmVar index -> context !! index
