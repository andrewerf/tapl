{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TupleSections #-}
module Lambda2.Lambda2
(
processString
)
where

import Lambda2.Core.AST
import Lambda2.Core.Core
import Lambda2.Core.Typing
import Lambda2.Core.Preprocess
import Lambda2.Parsing.Parser
import Lambda2.Core.BasicTypes

import Control.Applicative ( liftA2 )

import Foreign.C.Types
import Foreign.C.String


processString :: Context -> String -> Either String ( Term, Type )
processString ctx s = case ( maybeDesugared, maybeType ) of
  ( Left err, _ ) -> Left $ show err
  ( _, Left err ) -> Left err
--  ( Right term, _ ) -> liftA2 (,) ( eval ctx term ) maybeType
  ( Right term, _ ) -> ( term, ) <$> maybeType
  where
    maybeDesugared = ( desugar ctx.parse ) s
    maybeType = case maybeDesugared of
      ( Left err ) ->  Left $ show err
      ( Right term ) -> case typeof ctx term of
        ( Left err ) -> Left $ show err
        ( Right tp ) -> Right tp


processStringExported :: CString -> IO CString
processStringExported cStr = do
  str <- peekCString cStr
  let ctx = basicContext
  let processedStr = either id ( \(tm, tp) -> show (termToTermSimple ctx tm) ++ " : " ++ show (typeToTypeSimple ctx tp) ) ( processString ctx str )
  newCString processedStr

foreign export ccall processStringExported :: CString -> IO CString