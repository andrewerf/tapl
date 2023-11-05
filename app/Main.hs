module Main (main) where

import Lambda2.Core.AST
import Lambda2.Core.BasicTypes
import Lambda2.Lambda2
import qualified Lambda2.Core.CustomLLVM as LLVM

import Control.Monad.IO.Class
import Control.Monad.State

import Relude.Lifted.Env ( getArgs )
import Control.Applicative ( liftA2 )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C


-- Γ = β:*, γ:*, b:β, c:γ
testContext :: Context
testContext = extendContextWithTypeVar "Int" ( TpVar 0 ) KndStar $ extendContextWithTypeVar "Cont" ( TpVar 0 ) KndStar mempty
--  extendContextWithVar "bb" ( TpVar 3 ) $
--    extendContextWithVar "c" ( TpVar 1 ) $
--      extendContextWithVar "b" ( TpVar 1 ) $
--        extendContextWithTypeVar "γ" $
--          extendContextWithTypeVar "β" mempty

convertEither :: Either a (IO b) -> IO (Either a b)
convertEither = either (return . Left) (fmap Right)

main :: IO ()
main = do
  args <- getArgs
  maybeContentAndF <- case args of
--    [ inp ] -> do
--      fileContent <- readFile inp
--      return $ Right ( fileContent, \( tm, tp ) -> show ( termToTermSimple testContext tm ) ++ " : " ++ show ( typeToTypeSimple testContext tp ) )

    [ inp, "--compile" ] -> do 
      fileContent <- readFile inp
      return $ Right ( fileContent, \( tm, _ ) -> return $ C.pack . LLVM.state2code . snd $ runState ( LLVM.compileCps0 $ evalState ( LLVM.term2cps testContext tm ) LLVM.emptyFreshNameProviderState ) LLVM.zeroModuleState )
      
    _ -> return $ Left "Bad input"
  
  let processAndCompile ( fileContent, f ) = f <$> processString testContext fileContent
  r <- convertEither $ maybeContentAndF >>= processAndCompile
  case r of
    Right bs -> BS.putStr bs
    Left err -> putStr err