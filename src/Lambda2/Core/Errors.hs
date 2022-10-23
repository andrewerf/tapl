{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Lambda2.Core.Errors
where

import Lambda2.Core.AST
import Control.Monad.Free
import Data.Functor.Classes ( Eq1(..) )
import Text.Show.Deriving ( deriveShow1 )


data ErrorKind =
  NoVarInContext |

  BadHeadTerm |
  BadTailTerm |
  BadLeftTerm |
  BadRightTerm |

  BadHeadType |
  BadTailType |
  BadLeftType |
  BadRightType |
  TypesNotEq

  deriving ( Eq, Show )

data ErrorExprStructure termT typeT = ErrorExprStructure {
  kind    :: ErrorKind,
  context :: Context,
  terms :: [termT],
  types :: [typeT]
} deriving ( Eq )

instance forall termT typeT. Show ( ErrorExprStructure termT typeT ) where
  show ( ErrorExprStructure knd ctx _ _ ) = show knd ++ "\ncontext: " ++ show ctx


data Error expr next =
  Eq expr => DesugarError expr next |
  Eq expr => TypeError expr next

instance Eq1 ( Error expr ) where
  liftEq f ( DesugarError expr1 next1 ) ( DesugarError expr2 next2 ) = expr1 == expr2 && f next1 next2
  liftEq f ( TypeError expr1 next1 ) ( TypeError expr2 next2 ) = expr1 == expr2 && f next1 next2
  liftEq _ ( TypeError _ _ ) ( DesugarError _ _ ) = False
  liftEq _ ( DesugarError _ _ ) ( TypeError _ _ ) = False


$(deriveShow1 ''Error)


instance Functor ( Error expr ) where
  fmap f ( DesugarError e next) = DesugarError e $ f next
  fmap f ( TypeError e next) = TypeError e $ f next

data ErrorExpr =
 ErrorExprSimple ( ErrorExprStructure TermSimple TypeSimple ) |
 ErrorExpr       ( ErrorExprStructure Term Type )
 deriving ( Eq, Show )

type ErrorM = Free ( Error ErrorExpr )


makeError :: ( MonadFree f m, Functor f ) => ( t -> () -> f a ) -> t -> m a
makeError c e = liftF ( c e () )

makeSimpleDesugarError :: ErrorKind -> Context -> [TermSimple] -> [TypeSimple] -> ErrorM ()
makeSimpleDesugarError knd ctx tms tps = makeError DesugarError . ErrorExprSimple $ ErrorExprStructure{
  kind = knd,
  context = ctx,
  terms = tms,
  types = tps
  }

makeTypeError :: ErrorKind -> Context -> [Term] -> [Type] -> ErrorM ()
makeTypeError knd ctx tms tps = makeError TypeError . ErrorExpr $ ErrorExprStructure{
  kind = knd,
  context = ctx,
  terms = tms,
  types = tps
  }


showErrorPrefix :: String -> ErrorM next -> String
showErrorPrefix p err = p ++ ( \case
  Free ( DesugarError expr next ) -> "Desugar error: " ++ show expr ++ "  in:\n" ++ showErrorPrefix ( p ++ "\t" ) next
  Free ( TypeError expr next ) -> "Type error: " ++ show expr ++ "  in:\n" ++ showErrorPrefix ( p ++ "\t" ) next
  Pure _ -> "" ) err

showError :: ErrorM next -> String
showError = showErrorPrefix ""
