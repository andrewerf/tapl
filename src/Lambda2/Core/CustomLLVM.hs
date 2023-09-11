{-# LANGUAGE RecordWildCards #-}

module Lambda2.Core.CustomLLVM where

import Data.List
import Control.Monad.State

import qualified Lambda2.Core.AST as L
import qualified Lambda2.Core.Typing as L

import Debug.Trace


data Name
  = Name String
  | UnName Int
  deriving (Eq, Show)

data Type
  = TpInt8
  | TpIn32
  | TpPtr
  | TpVoid
  deriving (Eq, Show)

data Constant
  = ConstInt8 Int
  | ConstInt32 Int
  | ConstIntrinsic String
  deriving (Eq, Show)

data Op
  = OpLocalRef Type Name   -- %Name
  | OpGlobalRef Type Name  -- @Name
  | OpConstant Constant
  deriving (Eq, Show)

data Instr
  = Call
      Type  -- return type
      Op    -- func
      [Op]  -- args
  | Ret Op
  | RetVoid
  deriving (Eq, Show)


data Closure = Closure {
    func :: Op,
    capt :: Op
  }
  deriving (Eq, Show)

data FuncState = FuncState {
    funcName :: String,           -- Function name
    funcRet :: Type,              -- Return type
    funcArgs :: [Type],           -- Arg types
    funcInstructions :: [Instr]   -- All the instructions inside the block
  }
  deriving (Eq, Show)

zeroFuncState :: FuncState
zeroFuncState = FuncState {
    funcName = "func_name",
    funcRet = TpVoid,
    funcArgs = [],
    funcInstructions = []
  }

data ModuleState = ModuleState {
    moduleCurrentFunc :: Int,         -- Current function
    moduleFuncs :: [FuncState]        -- All the function in the module
  }
  deriving (Eq, Show)

zeroModuleState :: ModuleState
zeroModuleState = ModuleState {
    moduleCurrentFunc = 0,
    moduleFuncs = [zeroFuncState]
  }

type Module a = State ModuleState a


name2code :: Name -> String
name2code ( Name s ) = s
name2code ( UnName i ) = show i

type2code :: Type -> String
type2code TpVoid = "void"
type2code TpInt8 = "i8"
type2code TpIn32 = "i32"
type2code TpPtr = "ptr"

constant2code :: Constant -> String
constant2code ( ConstInt8 x ) = show x
constant2code ( ConstInt32 x ) = show x
constant2code ( ConstIntrinsic s ) = show s

constant2codeWithType :: Constant -> String
constant2codeWithType ( ConstInt8 x ) = "i8 " <> show x
constant2codeWithType ( ConstInt32 x ) = "i32 " <> show x
constant2codeWithType ( ConstIntrinsic s ) = s

op2code :: Op -> String
op2code ( OpLocalRef _ nm ) = "%" <> name2code nm
op2code ( OpGlobalRef _ nm ) = "@" <> name2code nm
op2code ( OpConstant c ) = constant2code c

op2codeWithType :: Op -> String
op2codeWithType ( OpLocalRef tp nm ) = type2code tp <> " %" <> name2code nm
op2codeWithType ( OpGlobalRef tp nm ) = type2code tp <> " @" <> name2code nm
op2codeWithType ( OpConstant c ) = constant2codeWithType c

instr2code :: Instr -> String
instr2code ( Call tp f args ) = "call " <> type2code tp <> " " <> op2code f <> " (" <> intercalate ", " ( map op2codeWithType args ) <> ")"
instr2code ( Ret op ) = "ret " <> op2code op
instr2code RetVoid = "ret void"


funcState2code :: FuncState -> String
funcState2code FuncState{..} =
  "define " <> type2code funcRet <> " @" <> funcName <> "(" <> intercalate ", " ( map ( \(t, i) -> type2code t <> " %" <> show i ) ( zip funcArgs [0..length funcArgs] ) ) <> "){\n"
    <> "\t" <> intercalate "\n\t" ( map instr2code funcInstructions )
    <> "\n}\n"

state2code :: ModuleState -> String
state2code ModuleState{ moduleFuncs=moduleFuncs } = intercalate "\n\n" $ map funcState2code moduleFuncs



transformIndex :: [a] -> Int -> ( a -> a ) -> [a]
transformIndex ( x : xs ) 0 f = f x : xs
transformIndex ( x : xs ) i f = x : transformIndex xs ( i - 1 ) f
transformIndex _ _ _ = error "replaceIndex is given a bad index"

replaceIndex :: [a] -> Int -> a -> [a]
replaceIndex lst i x = transformIndex lst i ( const x )

emit :: Instr -> Module ()
emit instr = do
  s <- get
  let k = moduleCurrentFunc s
  let currentFunc = moduleFuncs s !! k
  let newCurrentFunc = currentFunc{ funcInstructions=funcInstructions currentFunc ++ [instr] }
  put s{ moduleFuncs = replaceIndex ( moduleFuncs s ) k newCurrentFunc }
--trace ( "Index: " <> show k <> " list: " <> show ( moduleFuncs s ) ) $

emitFunc :: FuncState -> Module Int
emitFunc fn = do
  s <- get
  let n = ( length . moduleFuncs $ s )
  put s{ moduleFuncs = moduleFuncs s ++ [fn] }
  return n

setCurrentFunc :: Int -> Module ()
setCurrentFunc k = do
  s <- get
  put s{ moduleCurrentFunc = k }


cpsType2Type :: TypeCps -> Type
cpsType2Type TcInt = TpInt8
cpsType2Type TcPtr = TpPtr

cpsVar2Op :: TermCps -> Module Op
cpsVar2Op ( TmcVar x tp ) = return $ OpLocalRef ( cpsType2Type tp ) $ UnName x
cpsVar2Op ( TmcGlobalVar nm tp ) = return $ OpGlobalRef ( cpsType2Type tp ) $ Name nm
cpsVar2Op ( TmcConst x _ ) = return $ OpConstant ( ConstInt8 x )
cpsVar2Op tm = error $ "Called cpsVar2Op on a non-var term: " <> show tm

compileCps :: TermCps -> Module ()
compileCps ( TmcApp t1 t2 ) = do
  f <- cpsVar2Op t1
  x <- cpsVar2Op t2
  emit ( Call TpVoid f [x] )

compileCps ( TmcApp2 t1 t2 t3 ) = do
  f <- cpsVar2Op t1
  x <- cpsVar2Op t2
  y <- cpsVar2Op t3
  emit ( Call TpVoid f [x, y] )

compileCps ( TmcLet name argsTps body next ) = do
  let args = map cpsType2Type argsTps
  initialFunc <- gets moduleCurrentFunc -- remember the func we are currently in

  newFunc <- emitFunc zeroFuncState{ -- emit a new function
      funcName = name,
      funcArgs = args
    }
  setCurrentFunc newFunc  -- and set just emmited function to current
  compileCps body
  emit RetVoid
  setCurrentFunc initialFunc -- get back to the initial function
  compileCps next

compileCps tm = error $ "compileCps invoked with wrong input: " <> show tm


---



data TypeCps
  = TcInt
  | TcPtr
  deriving ( Eq, Show )

data TermCps
  = TmcVar Int TypeCps
  | TmcGlobalVar String TypeCps
  | TmcConst Int TypeCps
  | TmcApp TermCps TermCps
  | TmcApp2 TermCps TermCps TermCps
  | TmcLet
      String    -- name
      [TypeCps] -- arg types
      TermCps   -- body
      TermCps   -- next (only this is placed to local block)
  deriving ( Eq, Show )


data FreshNameProviderState = FreshNameProviderState {
    contCounter :: Int,
    funcCounter :: Int
  }

emptyFreshNameProviderState :: FreshNameProviderState
emptyFreshNameProviderState = FreshNameProviderState{ contCounter = 0, funcCounter = 0 }

type FreshNameProvider a = State FreshNameProviderState a

freshCont :: FreshNameProvider String
freshCont = do
  s <- get
  let name = "cont" <> show ( contCounter s )
  let newS = s{ contCounter = contCounter s + 1 }
  put newS
  return name

freshFunc :: String -> FreshNameProvider String
freshFunc hint = do
  s <- get
  let name = hint <> show ( contCounter s )
  let newS = s{ contCounter = contCounter s + 1 }
  put newS
  return name


type2cps :: L.Type -> TypeCps
type2cps ( L.TpVar{} ) = TcInt
type2cps ( L.TpArrow{} ) = TcPtr
type2cps _ = error "Unsupported type"


term2cps :: L.Context -> L.Term -> FreshNameProvider TermCps

term2cps ctx ( L.TmVar ( L.BoundVar i ) ) = return $ TmcApp ( TmcVar 0 TcPtr ) ( TmcVar i tp )
  where
    tp = type2cps $ case L.getVar ctx ( i - 1 ) of
      Nothing -> error $ "Could not find var in context: " <> show i
      Just t -> snd t

term2cps ctx ( L.TmVar ( L.DataVar ( L.TdInt x ) ) ) = return $ TmcApp ( TmcVar 0 TcPtr ) ( TmcConst x TcInt )

term2cps ctx ( L.TmAbs varName varType ( L.TailAbs tl ) ) = do
  f <- freshFunc "v"
  t <- term2cps ctx $ L.shift0 1 tl
  return $ TmcLet f [TcPtr, type2cps varType] t ( TmcApp ( TmcVar 0 TcPtr ) ( TmcGlobalVar f TcPtr ) )

term2cps ctx tm@( L.TmApp f x ) = do
  let x_type = type2cps $
        case L.typeof ctx x of
          Left err -> error $ show err
          Right tp -> tp
  let res_type = type2cps $
        case L.typeof ctx tm of
           Left err -> error $ show err
           Right tp -> tp

  u <- freshFunc "u"
  g <- freshFunc "g"
  c1 <- freshCont
  c2 <- freshCont
  f_cps <- term2cps ctx f
  x_cps <- term2cps ctx x
  return $
    TmcLet u [TcPtr] f_cps (
        TmcLet c1 [TcPtr] (
            TmcLet g [TcPtr] x_cps (
              TmcLet c2 [res_type] ( TmcApp2 ( TmcVar 1 TcPtr ) ( TmcVar 0 x_type ) ( TmcVar 2 TcInt ) )
                ( TmcApp ( TmcGlobalVar g TcPtr ) ( TmcGlobalVar c2 TcPtr ) )
            )
        )
        ( TmcApp ( TmcGlobalVar u TcPtr ) ( TmcGlobalVar c1 TcPtr ) )
     )

term2cps _ tm = error $ "Unsupported term " <> show tm



exampleContext = L.extendContextWithTypeVar "Int" ( L.TpVar 0 ) L.KndStar mempty
--example = L.TmAbs "x" ( L.TpVar 0 ) ( L.TailAbs ( L.TmVar ( L.BoundVar 0 ) ) )
example = L.TmApp
            ( L.TmAbs "x" ( L.TpVar 0 ) ( L.TailAbs ( L.TmVar ( L.BoundVar 0 ) ) ) )
            ( L.TmVar ( L.DataVar ( L.TdInt 10 ) ) )


test :: (TermCps, FreshNameProviderState)
test = runState ( term2cps exampleContext example ) emptyFreshNameProviderState

compiledTest :: ( (), ModuleState )
compiledTest = runState ( compileCps $ fst test ) zeroModuleState