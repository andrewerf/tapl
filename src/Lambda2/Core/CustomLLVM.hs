{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Lambda2.Core.CustomLLVM where

import Data.List
import Control.Monad.State

import qualified Lambda2.Core.AST as L
import qualified Lambda2.Core.Typing as L

import Text.RawString.QQ

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
  | TpStruct Name
  deriving (Eq, Show)

data Constant
  = ConstInt8 Int
  | ConstInt32 Int
  | ConstIntrinsic Type String
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
  | Assign Op Instr
  | Gep Type [Op]
  | Load Type Op
  | Store Op Op
  | Alloca Type
  | Ret Op
  | RetVoid
  deriving (Eq, Show)


data VarWithType = VarWithType Int Type | VarWithTypeWithDefault Int Type Constant
  deriving ( Eq, Show )
  
toList :: [VarWithType] -> [(Int, Type)]
toList = map $ \case
  VarWithType i tp -> ( i, tp )
  VarWithTypeWithDefault i tp _ -> ( i, tp )
  
fromList :: [(Int, Type)] -> [VarWithType]
fromList = map $ \(i, tp) -> VarWithType i tp

getTypes :: [VarWithType] -> [Type]
getTypes = map snd . toList
  
getVars :: [VarWithType] -> [Int]
getVars = map fst . toList


data ClosureType = ClosureType {
    closName :: String,
    closFunc :: FuncState,
    closStack :: [VarWithType]      -- free variables that are used in closFunc (relative)
  }
  deriving (Eq, Show)

data FuncState = FuncState {
    funcName :: String,           -- Function name
    funcRet :: Type,              -- Return type
    funcArgs :: [Type],           -- Arg types
    funcInstructions :: [Instr],  -- All the instructions inside the block
    funcTemporariesCounter :: Int -- Number of temporaries used
  }
  deriving (Eq, Show)

zeroFuncState :: FuncState
zeroFuncState = FuncState {
    funcName = "fun",
    funcRet = TpVoid,
    funcArgs = [],
    funcInstructions = [],
    funcTemporariesCounter = 0
  }

mainFuncState :: FuncState
mainFuncState = zeroFuncState{
    funcName = "main",
    funcRet = TpIn32
  }

data ModuleState = ModuleState {
    moduleCurrentFunc :: Int,         -- Current function
    moduleFuncs :: [FuncState],       -- All the function in the module
    moduleClTypes :: [ClosureType]    -- All the closures' types in the module
  }
  deriving (Eq, Show)

zeroModuleState :: ModuleState
zeroModuleState = ModuleState {
    moduleCurrentFunc = 0,
    moduleFuncs = [mainFuncState],
    moduleClTypes = [ClosureType{ closFunc=mainFuncState, closName="main_cl", closStack=[VarWithTypeWithDefault 0 TpPtr ( ConstIntrinsic TpPtr "@print" )]}]
  }

type Module a = State ModuleState a


name2code :: Name -> String
name2code ( Name s ) = s
name2code ( UnName i ) = "a" <> show i

type2code :: Type -> String
type2code TpVoid = "void"
type2code TpInt8 = "i8"
type2code TpIn32 = "i32"
type2code TpPtr = "ptr"
type2code ( TpStruct s ) = "%struct." <> name2code s

type2defaultValue :: Type -> Op
type2defaultValue TpVoid = OpConstant $ ConstIntrinsic TpVoid "void"
type2defaultValue TpInt8 = OpConstant $ ConstInt8 0
type2defaultValue TpIn32 = OpConstant $ ConstInt32 0
type2defaultValue TpPtr = OpConstant $ ConstIntrinsic TpPtr "null"
type2defaultValue tp = error $ "Could not get default value for " <> show tp

constant2code :: Constant -> String
constant2code ( ConstInt8 x ) = show x
constant2code ( ConstInt32 x ) = show x
constant2code ( ConstIntrinsic _ s ) = s

constant2codeWithType :: Constant -> String
constant2codeWithType ( ConstInt8 x ) = "i8 " <> show x
constant2codeWithType ( ConstInt32 x ) = "i32 " <> show x
constant2codeWithType ( ConstIntrinsic tp s ) = type2code tp <> " " <> s

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
instr2code ( Ret op ) = "ret " <> op2codeWithType op
instr2code RetVoid = "ret void"
instr2code ( Assign op instr ) = op2code op <> " = " <> instr2code instr
instr2code ( Load tp op ) = "load " <> type2code tp <> ", " <> op2codeWithType op
instr2code ( Store op1 op2 ) = "store " <> op2codeWithType op1 <> ", " <> op2codeWithType op2
instr2code ( Alloca tp ) = "alloca " <> type2code tp
instr2code ( Gep tp ops ) = "getelementptr " <> type2code tp <> ", " <> intercalate ", " ( map op2codeWithType ops )


funcState2code :: FuncState -> String
funcState2code FuncState{..} =
  "define " <> type2code funcRet <> " @" <> funcName <> "(" <> intercalate ", " ( map ( \(t, i) -> type2code t <> " %a" <> show i ) ( zip funcArgs $ reverse [0..length funcArgs - 1] ) ) <> "){\n"
    <> "    " <> intercalate "\n    " ( map instr2code funcInstructions )
    <> "\n}\n"

cl2code :: ClosureType -> String
cl2code ClosureType{..} =
  let name = Name closName in
  let tp = TpStruct name in
  let fn = OpGlobalRef TpPtr $ Name ( clName2fnName closName ) in
  let stackTypes = TpPtr : getTypes closStack in
  let stack = fn : map ( \case 
       VarWithType _ a -> type2defaultValue a
       VarWithTypeWithDefault _ _ a -> OpConstant a ) closStack in
  type2code tp <> " = type {" <> intercalate ", " ( map type2code stackTypes ) <> "}\n" <>
  op2code ( OpGlobalRef tp name ) <> " = global " <> type2code tp <> " {" <>
    intercalate ", " ( map op2codeWithType stack ) <> "}"


state2code :: ModuleState -> String
state2code ModuleState{..} =
  ( [r|

%struct.callable = type {ptr, ptr}

%struct.print = type {ptr}
@print = global %struct.print {ptr @print_fun}

@.str = private unnamed_addr constant [15 x i8] c"value is : %d\0A\00", align 1
declare i32 @printf(i8*, ...)
define void @print_fun(i8 %0){
   call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([13 x i8], [15 x i8]* @.str, i32 0, i32 0), i8 %0)
   ret void
}

|] :: String ) <>
  ( intercalate "\n\n" $ map cl2code moduleClTypes ++ map funcState2code moduleFuncs )



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

emitCl :: ClosureType -> Module ()
emitCl cl = do
  s <- get
  put s{ moduleClTypes = moduleClTypes s ++ [cl] }

setCurrentFunc :: Int -> Module ()
setCurrentFunc k = do
  s <- get
  put s{ moduleCurrentFunc = k }

getCurrentFunc :: Module FuncState
getCurrentFunc = do
  k <- gets moduleCurrentFunc
  funcs <- gets moduleFuncs
  return $ funcs !! k

getTemporaryName :: Module Name
getTemporaryName = do
  f <- getCurrentFunc
  let i = funcTemporariesCounter f
  -- modify temporaries counter
  k <- gets moduleCurrentFunc
  s <- get
  let newCurrentFunc = f{funcTemporariesCounter = i + 1}
  put s{moduleFuncs = replaceIndex ( moduleFuncs s ) k newCurrentFunc}
  --
  return . Name $ "t" <> show i

getClosureTypeName :: ClosureType -> Name
getClosureTypeName ClosureType{..} = Name closName

getCallableType :: Type
getCallableType = TpStruct $ Name "callable"

getOpType :: Op -> Type
getOpType ( OpGlobalRef tp _ ) = tp
getOpType ( OpLocalRef tp _ ) = tp
getOpType ( OpConstant ( ConstInt8 _ ) ) = TpInt8
getOpType ( OpConstant ( ConstInt32 _ ) ) = TpIn32
getOpType ( OpConstant ( ConstIntrinsic tp _ ) ) = tp


fnName2clName :: String -> String
fnName2clName "main" = "main_cl"
fnName2clName s = reverse . drop 4 . reverse $ s

clName2fnName :: String -> String
clName2fnName "main_cl" = "main"
clName2fnName s = s <> "_fun"


zipWithLength :: [a] -> [(a, Int)]
zipWithLength lst = zip lst $ [0..length lst]


getArgOrClVar :: Int -> Type -> Module Op
getArgOrClVar x tp = do
  currentFunc <- getCurrentFunc
  let fn_name = funcName currentFunc
  let argsCount = length ( funcArgs currentFunc )
  if x < argsCount then   -- if this is a bound variable, then just return a local reference
     return $ OpLocalRef tp $ UnName x
   else do                -- otherwise, extract the variable from current closure's stack
     t_adr <- getTemporaryName
     let t_adr_op = OpLocalRef TpPtr t_adr

     let cl_name = Name $ fnName2clName fn_name
     let cltp = TpStruct cl_name
     let cl = OpGlobalRef TpPtr cl_name
     n <- ( snd . justOrErr "getArgOrClVar" . find ( ( == x ) . fst . fst ) . zipWithLength . toList . closStack
            . head . filter ( \c -> ( funcName . closFunc $ c ) == fn_name ) )
              <$> gets moduleClTypes


     emit $ Assign t_adr_op ( Gep cltp [cl, OpConstant $ ConstInt32 0, OpConstant $ ConstInt32 ( 1 + n )] )
     t <- getTemporaryName
     let t_op = OpLocalRef tp t
     emit $ Assign t_op ( Load tp t_adr_op )
     return t_op


initClosure :: ClosureType -> Module ()
initClosure cl@ClosureType{..} = do
  -- cl_ptr corresponds to the function name
  let cl_ptr = Name closName
  let cl_ptr_op = OpGlobalRef TpPtr cl_ptr
  --
  -- copy each variable from the local "scope"
  mapM_ ( f cl_ptr_op ) ( zip ( toList closStack ) [1..length closStack + 1] )
  where
  cl_type_name = getClosureTypeName cl
  cl_type = TpStruct cl_type_name
  argsCount = length . funcArgs $ closFunc

  f :: Op -> ( ( Int, Type ), Int ) -> Module ()
  f cl_ptr_op ( ( var, tp ), i ) = do
    t_adr <- getTemporaryName
    let t_adr_op = OpLocalRef TpPtr t_adr
    emit $ Assign t_adr_op ( Gep cl_type [cl_ptr_op, OpConstant $ ConstInt32 0, OpConstant $ ConstInt32 i] )
    src_op <- getArgOrClVar ( var - argsCount ) tp
    emit $ Store src_op t_adr_op

callClosure :: Op -> [Op] -> Module ()
callClosure cl args = do
  -- To call a closure, we need to extract the function ptr (first struct member) from it, and call this function
  f_adr <- getTemporaryName
  let f_adr_op = OpLocalRef TpPtr f_adr
  emit $ Assign f_adr_op ( Gep getCallableType [cl, OpConstant $ ConstInt32 0, OpConstant $ ConstInt32 0] )
  f <- getTemporaryName
  let f_op = OpLocalRef TpPtr f
  emit $ Assign f_op ( Load TpPtr f_adr_op )
  emit $ Call TpVoid f_op args


cpsType2Type :: TypeCps -> Type
cpsType2Type TcInt = TpInt8
cpsType2Type TcPtr = TpPtr

cpsVar2Op :: TermCps -> Module Op
cpsVar2Op ( TmcVar x tpCps ) = getArgOrClVar x ( cpsType2Type tpCps )
cpsVar2Op ( TmcGlobalVar nm tp ) = return $ OpGlobalRef ( cpsType2Type tp ) $ Name nm
cpsVar2Op ( TmcConst x _ ) = return $ OpConstant ( ConstInt8 x )
cpsVar2Op tm = error $ "Called cpsVar2Op on a non-var term: " <> show tm

compileCps :: Int -> TermCps -> Module ()
compileCps _ ( TmcApp t1 t2 ) = do
  f <- cpsVar2Op t1
  x <- cpsVar2Op t2
  callClosure f [x]

compileCps _ ( TmcApp2 t1 t2 t3 ) = do
  f <- cpsVar2Op t1
  x <- cpsVar2Op t2
  y <- cpsVar2Op t3
  callClosure f [x, y]

compileCps depth ( TmcLet name argsTps body next ) = do
  let args = map cpsType2Type argsTps
  let fvs = fv2cps ( length argsTps ) body

  let newFunc = zeroFuncState{
      funcName = clName2fnName name,
      funcArgs = args
    }
  let newClType = ClosureType{
      closName = name,
      closFunc = newFunc,
      closStack = fromList $ zip ( map fst fvs ) ( map ( cpsType2Type . snd ) fvs )
    }

  initialFunc <- gets moduleCurrentFunc -- remember the func we are currently in
  newFuncNum <- emitFunc newFunc -- emit a new function
  emitCl newClType -- and its closure
  initClosure newClType -- Closures are unique global objects.
                        -- This calls initializes (copies from the scope) such an object for the just emitted closure type

  setCurrentFunc newFuncNum  -- set current function to just emitted one
  compileCps ( depth + 1 ) body
  emit RetVoid
  setCurrentFunc initialFunc -- get back to the initial function
  compileCps depth next

compileCps _ tm = error $ "compileCps invoked with wrong input: " <> show tm


compileCps0 :: TermCps -> Module ()
compileCps0 tm = do
  compileCps 0 tm
  emit $ Ret ( OpConstant $ ConstInt32 0 )

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
  deriving Eq


cpsShow :: TermCps -> State String String
cpsShow ( TmcVar x tp ) = do
  t <- get
  return $ t <> "TmcVar " <> show x <> " " <> show tp
cpsShow ( TmcGlobalVar x tp ) = do
  t <- get
  return $ t <> "TmcGlobalVar " <> show x <> " " <> show tp
cpsShow ( TmcConst x tp ) = do
  t <- get
  return $ t <> "TmcConst " <> show x <> " " <> show tp
cpsShow ( TmcApp t1 t2 ) = do
   t <- get
   return $ t <> "TmcApp " <> show t1 <> " " <> show t2
cpsShow ( TmcApp2 t1 t2 t3 ) = do
   t <- get
   return $ t <> "TmcApp2 " <> show t1 <> " " <> show t2 <> " " <> show t3
cpsShow ( TmcLet name args body next ) = do
   t <- get
   let s = t <> "TmcLet " <> name <> " " <> show args <> "\n" <> t <> "(\n"
   put $ t <> "\t"
   sBody <- cpsShow body
   sNext <- cpsShow next
   put t
   let ss = s <> sBody <> "\n" <> t <> ")\n" <> t <> "(\n" <> sNext <> "\n" <> t <> ")\n"
   return ss

instance Show TermCps where
  show = fst . flip runState "" .  cpsShow


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
  let name = hint <> show ( funcCounter s )
  let newS = s{ funcCounter = funcCounter s + 1 }
  put newS
  return name


justOrErr :: String -> Maybe a -> a
justOrErr comment Nothing = error $ "justOrErr called on Nothing, " <> comment
justOrErr _ ( Just x ) = x

--fv2cps :: L.Context -> L.Term -> [(Int, TypeCps)]
--fv2cps ctx tm = justOrErr "fv2cps" $ map ( \(i, t) -> ( i, type2cps t ) ) <$> ( trace ( "tm: " <> show tm <> " ctx: " <> show ctx ) $  L.fv ctx tm )

fv2cps :: Int ->  TermCps -> [(Int, TypeCps)]
fv2cps c ( TmcVar k tp )
 | k < c = []
 | otherwise = [(k, tp)]
fv2cps _ TmcGlobalVar{} = []
fv2cps _ TmcConst{} = []
fv2cps c ( TmcApp f x ) = fv2cps c f ++ fv2cps c x
fv2cps c ( TmcApp2 f x y ) = fv2cps c f ++ fv2cps c x ++ fv2cps c y
fv2cps c ( TmcLet _ args body next ) = ( filterFv c . shiftFv ( -argsLength ) $ fv2cps ( c + argsLength ) body ) ++ fv2cps c next
  where
    argsLength = length args
    shiftFv :: Int -> [(Int, TypeCps)] -> [(Int, TypeCps)]
    shiftFv x = map ( \(i, t) -> (i + x, t) )

    filterFv :: Int -> [(Int, TypeCps)] -> [(Int, TypeCps)]
    filterFv ccc = filter ( ( >= ccc ) . fst )

type2cps :: L.Context -> L.Type -> TypeCps
type2cps ctx ( L.TpVar i ) =
  let typeName = justOrErr "type2cps on TpVar" $ L.getTypeVar ctx i in
    case typeName of
      "Int" -> TcInt
      s -> error $ "Type " <> s <> " is not convertible to CPS"
type2cps _ ( L.TpArrow{} ) = TcPtr
type2cps _ tp = error $ "Unsupported type: " <> show tp


shiftCps :: Int -> Int -> TermCps -> TermCps
shiftCps c s ( TmcVar k tp )
 | k < c = TmcVar k tp
 | otherwise = TmcVar ( k + s ) tp
shiftCps _ _ tm@TmcGlobalVar{} = tm
shiftCps _ _ tm@TmcConst{} = tm
shiftCps c s ( TmcApp f x ) = TmcApp ( shiftCps c s f ) ( shiftCps c s x )
shiftCps c s ( TmcApp2 f x y ) = TmcApp2 ( shiftCps c s f ) ( shiftCps c s x ) ( shiftCps c s y )
shiftCps c s ( TmcLet name args body next ) = TmcLet name args ( shiftCps ( c + length args ) s body ) ( shiftCps c s next )


term2cps :: L.Context -> L.Term -> FreshNameProvider TermCps

term2cps ctx ( L.TmVar ( L.BoundVar i ) ) = return $ TmcApp ( TmcVar 0 TcPtr ) ( TmcVar i tp )
  where
    tp = type2cps ctx $ case L.getVar ctx i of
      Nothing -> error $ "Could not find var in context: " <> show i
      Just t -> snd t

term2cps ctx ( L.TmVar ( L.DataVar ( L.TdInt x ) ) ) = return $ TmcApp ( TmcVar 0 TcPtr ) ( TmcConst x TcInt )

term2cps ctx ( L.TmAbs varName varType ( L.TailAbs tl ) ) = do
  let contType = justOrErr "term2cps on TmAbs" $ L.getVarIndex ctx "Cont"
  let extendedContext = L.extendContextWithVar "cont" ( L.TpVar ( contType + 1 ) ) $ L.extendContextWithVar varName varType ctx
  t <- term2cps extendedContext ( L.shift0 1 tl )
  f <- freshFunc "v"
  return $ TmcLet f [type2cps ctx varType, TcPtr] t ( TmcApp ( TmcVar 0 TcPtr ) ( TmcGlobalVar f TcPtr ) )

term2cps ctx tm@( L.TmApp f x ) = do
  let x_type = type2cps ctx $
        case L.typeof ctx x of
          Left err -> error $ "TmApp " <> show f <> " " <> show x <> " " <> show ctx
          Right tp -> tp

  u <- freshFunc "u"
  g <- freshFunc "g"
  c1 <- freshCont
  c2 <- freshCont
  f_cps <- shiftCps 1 1 <$> term2cps ctx f
  x_cps <- shiftCps 1 2 <$> term2cps ctx x
  return $
    TmcLet u [TcPtr] f_cps (
        TmcLet c1 [TcPtr] (
            TmcLet g [TcPtr] x_cps (
              TmcLet c2 [x_type] ( TmcApp2 ( TmcVar 1 TcPtr ) ( TmcVar 0 x_type ) ( TmcVar 2 TcPtr )  )
                ( TmcApp ( TmcGlobalVar g TcPtr ) ( TmcGlobalVar c2 TcPtr ) )
            )
        )
        ( TmcApp ( TmcGlobalVar u TcPtr ) ( TmcGlobalVar c1 TcPtr ) )
     )

term2cps _ tm = error $ "Unsupported term " <> show tm



exampleContext = L.extendContextWithTypeVar "Int" ( L.TpVar 0 ) L.KndStar mempty
--example = L.TmAbs "x" ( L.TpVar 0 ) ( L.TailAbs ( L.TmVar ( L.BoundVar 0 ) ) )
--example = L.TmApp
--            ( L.TmAbs "x" ( L.TpVar 0 ) ( L.TailAbs ( L.TmVar ( L.BoundVar 0 ) ) ) )
--            ( L.TmVar ( L.DataVar ( L.TdInt 10 ) ) )
--example = L.TmApp ( L.TmApp
--            ( L.TmAbs "x" ( L.TpVar 0 ) ( L.TailAbs ( L.TmAbs "y" ( L.TpVar 1 ) ( L.TailAbs ( L.TmVar ( L.BoundVar 0 ) ) ) ) ) )
--            ( L.TmVar ( L.DataVar ( L.TdInt 10 ) ) ) ) ( L.TmVar ( L.DataVar ( L.TdInt 15 ) ) )
example = L.TmApp ( L.TmApp ( L.TmApp
            ( L.TmAbs "z" ( L.TpVar 0 ) ( L.TailAbs ( L.TmAbs "x" ( L.TpVar 0 ) ( L.TailAbs ( L.TmAbs "y" ( L.TpVar 1 ) ( L.TailAbs ( L.TmVar ( L.BoundVar 1 ) ) ) ) ) ) ) )
            ( L.TmVar ( L.DataVar ( L.TdInt 10 ) ) ) ) ( L.TmVar ( L.DataVar ( L.TdInt 15 ) ) ) ) ( L.TmVar ( L.DataVar ( L.TdInt 99 ) ) )

test :: (TermCps, FreshNameProviderState)
test = runState ( term2cps exampleContext example ) emptyFreshNameProviderState

compiledTest :: ( (), ModuleState )
compiledTest = runState ( compileCps0 $ fst test ) zeroModuleState