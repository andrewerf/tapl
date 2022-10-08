{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
module SimpleLambdaErrors where

import Control.Applicative ( liftA2 )
import Data.Default

import SimpleLambdaAST



-- Instances of this class maybe showed like a tree structures
class ShowTree t where
  showTree :: Int -> t -> String
  toLines :: ( t -> String ) -> t -> [String]

  showTree level t = "\n" ++ ( foldl1 ( \a b -> a ++ "\n" ++ b )
                      $ map ( \s -> foldl1 (++) ( replicate level "\t" ) ++ s )
                      $ toLines ( showTree ( level + 1 ) ) t )


-- Contains all the type derivation tree
-- from top to error
data TypeError termType
  = Show termType => AppTypeError {
    leftTerm    :: termType,
    rightTerm   :: termType,
    leftType    :: Either ( TypeError termType ) Type,
    rightType   :: Either ( TypeError termType ) Type
  }
  | Show termType => AbsTypeError {
    varName     :: String,
    tailTerm    :: TypeError termType
  }
  | VarTypeError {
    varIndex    :: Maybe Int,
    context     :: Context
  }

type TypeErrorTerm = TypeError Term

data DesugarError
  = AppDesugarError {
    leftTermSimple  :: TermSimple,
    rightTermSimple :: TermSimple,
    leftTerm        :: Either DesugarError Term,
    rightTerm       :: Either DesugarError Term
  }
  | VarDesugarError {
    varName     :: String,
    context     :: Context
  }
  | AbsDesugarError {
    varName     :: String,
    tailTerm    :: Either DesugarError Term
  }
  | LetDesugarError {
    varName         :: String,
    headTermSimple  :: TermSimple,
    tailTermSimple  :: TermSimple,
    headTerm        :: Either DesugarError Term,
    tailTerm        :: Either DesugarError Term,
    headType        :: Either TypeErrorTerm Type
  }

showContext :: Context -> String
showContext ctx = foldl1 ( \a b -> a ++ ", " ++ b ) ( map show ctx )

showEither :: forall a e.Show a => ( e -> String ) -> Either e a -> String
showEither _ ( Right t ) = show t
showEither showNext ( Left err ) = showNext err

instance Show termType => ShowTree ( TypeError termType ) where
  toLines showNext t =
    case t of
      VarTypeError{varIndex = varIndex, context = context} -> ["Var error",
                                                               "Index: " ++ show varIndex,
                                                               "Context: " ++ showContext context]
      AbsTypeError{varName = varName, tailTerm = tailTerm} -> ["Abs error",
                                                               "Var name: " ++ varName,
                                                               "Tail: " ++ showNext tailTerm]
      AppTypeError{leftTerm = leftTerm, rightTerm = rightTerm, leftType = leftType, rightType = rightType}
                                                           -> ["App error",
                                                               "Left term: " ++ show leftTerm,
                                                               "Right term: " ++ show rightTerm,
                                                               "Left type: " ++ showEither showNext leftType,
                                                               "Right type: " ++ showEither showNext rightType]

instance ShowTree DesugarError where
  toLines _ VarDesugarError{varName = varName, context = context}
    = ["Var error", "Var name: " ++ varName, "Context: " ++ showContext context]
  toLines showNext AbsDesugarError{varName = varName, tailTerm = tailTerm}
    = ["Abs error", "Var name: " ++ varName, "Tail: " ++ showEither showNext tailTerm]
  toLines showNext AppDesugarError{leftTermSimple = leftTermSimple, rightTermSimple = rightTermSimple, leftTerm = leftTerm, rightTerm = rightTerm}
    = ["App error", "Left term simple: " ++ show leftTermSimple, "Right term simple: " ++ show rightTermSimple,
       "Left term: " ++ showEither showNext leftTerm, "Right term: " ++ showEither showNext rightTerm]
  toLines showNext LetDesugarError{varName = varName, headTermSimple = headTermSimple, tailTermSimple = tailTermSimple,
                                   headTerm = headTerm, tailTerm = tailTerm, headType = headType}
    = ["Let error", "Head term simple: " ++ show headTermSimple, "Tail term simple: " ++ show tailTermSimple,
       "Head term: " ++ showEither showNext headTerm, "Tail term: " ++ showEither showNext tailTerm, "Head type: " ++ showEither showNext tailTerm]


instance Show termType => Show ( TypeError termType ) where
  show = showTree 1

instance Show DesugarError where
  show = showTree 1
