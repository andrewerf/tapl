{-# LANGUAGE LambdaCase #-}
module Lambda2.Core.AST
  (
  Kinds(..),
  TypeSimple(..),
  TermSimple(..),
  Type(..),
  Term(..),
  TermData(..),
  VarBinding(..),
  VarBindingInt,
  VarBindingString,
  AbsBinding(..),
  AbsBindingTerm,
  AbsBindingTermSimple,
  Context,
  ContextMember,
  shiftType0,
  shift0,
  getVarIndex,
  getType,
  getVar,
  getTypeVar,
  extendContextWithVar,
  extendContextWithTypeVar,
  termToTermSimple,
  typeToTypeSimple,
  ShowWithContext(..)
  )
where


import Relude.List
import Data.List ( find, elemIndex )
import Control.Applicative ( liftA2 )




data TermData =
  TdInt Int |
  TdFloat Float |
  TdString String
  deriving ( Eq, Show )

data VarBinding a =
  BoundVar a |
  DataVar TermData
  deriving ( Eq, Show )

type VarBindingInt = VarBinding Int
type VarBindingString = VarBinding String

-- function and identifier
data AbsBinding a b =
  TailAbs a |
  ActiveAbs ( Term -> Term ) b ( Maybe String )

type AbsBindingTerm = AbsBinding Term Type
type AbsBindingTermSimple = AbsBinding TermSimple TypeSimple

data Kinds =
  KndStar

data TypeSimple =
  TpsVar String |
  TpsArrow TypeSimple TypeSimple |
  TpsPoly String TypeSimple
  deriving ( Eq )

data TermSimple =
  TmsVar VarBindingString |
  TmsAbs String TypeSimple AbsBindingTermSimple |
  TmsApp TermSimple TermSimple |
  TmsLet String TermSimple TermSimple |
  TmsPoly String TermSimple |
  TmsType TypeSimple
  deriving ( Eq )

data Type =
  TpVar Int |
  TpArrow Type Type |
  TpPoly String Type
  deriving ( Eq, Show )

data Term =
  TmVar VarBindingInt |
  TmAbs String Type AbsBindingTerm |
  TmApp Term Term |
  TmPoly String Term |
  TmType Type
  deriving ( Eq, Show )


-- Context member is either "var:type" pair or "type:*" pair
-- Note that indices of types in the context are relative to the var
-- For instance a context Γ = β:*, γ:*, b:β, c:γ is encoded as
-- CmType "β", CmType "γ", CmVar "b" _ ( TpVar 2 ), CmVar "c" _ ( TpVar 2 )
--                    ^^^ ---------------------------------------- ^^^
--                                      distance = 1
--        ^^^ ------------------------------ ^^^
--                    distance = 1
--
-- Therefore when extracting a var:type pair from context, we should shift
-- all the types by pair index in the context.
data ContextMember = CmVar String Type | CmType String deriving ( Eq )
newtype Context = Context [ContextMember] deriving ( Eq )

instance Semigroup Context where
  ( Context ctx1 ) <> ( Context ctx2 ) = Context ( ctx1 <> ctx2 )

instance Monoid Context where
  mempty = Context []


getName :: ContextMember -> String
getName ( CmVar s _ ) = s
getName ( CmType s ) = s

getNames :: Context -> [String]
getNames ( Context ctx ) = map getName ctx


-- Shifts type preserving bound variables
--          cutoff  shift
shiftType :: Int -> Int -> Type -> Type
shiftType c s = \case
  TpVar j -> if j < c then TpVar j else TpVar ( j + s )
  TpArrow tp1 tp2 -> TpArrow ( shiftType c s tp1 ) ( shiftType c s tp2 )
  TpPoly name tp -> TpPoly name $ shiftType ( c + 1 ) s tp


shiftType0 :: Int -> Type -> Type
shiftType0 = shiftType 0


-- Shifts term (and types in it) preserving bound variables
--      cutoff  shift
shift :: Int -> Int -> Term -> Term
shift c s = \case
  TmVar ( BoundVar k )
    | k < c -> TmVar $ BoundVar k
    | otherwise -> TmVar $ BoundVar ( k + s )
  TmVar ( DataVar d ) -> TmVar $ DataVar d
  TmAbs varName varType ( TailAbs tailTerm ) -> TmAbs varName ( shiftType c s varType ) $ TailAbs ( shift ( c + 1 ) s tailTerm )
  TmAbs varName varType ( ActiveAbs f tp n ) -> TmAbs varName varType $ ActiveAbs f ( shiftType c s tp ) n
  TmApp t1 t2 -> TmApp ( shift c s t1 ) ( shift c s t2 )
  TmPoly typeVarName tm -> TmPoly typeVarName ( shift ( c + 1 ) s tm )
  TmType tp -> TmType ( shiftType c s tp )

shift0 :: Int -> Term -> Term
shift0 = shift 0

getVarIndex :: Context -> String -> Maybe Int
getVarIndex ctx varName = elemIndex varName $ getNames ctx


getType :: Context -> String -> Maybe Type
getType ctx varName =
  snd <$> ( getVarIndex ctx varName >>=
    getVar ctx )

getVar :: Context -> Int -> Maybe ( String, Type )
getVar ( Context ctx ) i = ( ctx !!? i ) >>=
  \case
    CmVar str tp -> Just (str, shiftType0 ( i + 1 ) tp)
    _ -> Nothing

getTypeVar :: Context -> Int -> Maybe String
getTypeVar ( Context ctx ) i = ( ctx !!? i ) >>=
  \case
    CmType s -> Just s
    _ -> Nothing

extendContextWithTypeVar :: String -> Context -> Context
extendContextWithTypeVar s ( Context ctx ) = Context ( CmType s : ctx )

extendContextWithVar :: String -> Type -> Context -> Context
extendContextWithVar s t ( Context ctx ) = Context ( CmVar s t : ctx )

typeToTypeSimple :: Context -> Type -> Maybe TypeSimple
typeToTypeSimple ctx =
  \case
    TpVar i -> TpsVar <$> getTypeVar ctx i
    TpArrow t1 t2 -> liftA2 TpsArrow ( typeToTypeSimple ctx t1  ) ( typeToTypeSimple ctx t2  )
    TpPoly s t -> TpsPoly s <$> typeToTypeSimple ( extendContextWithTypeVar s ctx ) t

termToTermSimple :: Context -> Term -> Maybe TermSimple
termToTermSimple ctx =
  \case
    TmVar ( BoundVar i ) -> TmsVar . BoundVar . fst <$> getVar ctx i
    TmVar ( DataVar d ) -> Just $ TmsVar $ DataVar d
    TmAbs s tp ( TailAbs tm ) -> liftA2 ( \tp' tm' -> TmsAbs s tp' ( TailAbs tm' ) ) ( typeToTypeSimple ctx tp ) ( termToTermSimple ( extendContextWithVar s tp ctx ) tm )
    TmAbs s tp ( ActiveAbs _ tpAbs n ) -> liftA2 ( \tp' tpAbs' -> TmsAbs s tp' ( ActiveAbs id tpAbs' n ) ) ( typeToTypeSimple ctx tp ) ( typeToTypeSimple ctx tpAbs )
    TmApp t1 t2 -> liftA2 TmsApp ( termToTermSimple ctx t1 ) ( termToTermSimple ctx  t2 )
    TmPoly s t -> TmsPoly s <$> termToTermSimple ( extendContextWithTypeVar s ctx ) t
    TmType t -> TmsType <$> typeToTypeSimple ctx t


instance ( Eq a, Eq b ) => Eq ( AbsBinding a b ) where
  (==) ( TailAbs t1 ) ( TailAbs t2 ) = t1 == t2
  (==) ( ActiveAbs _ tp1 n1 ) ( ActiveAbs _ tp2 n2 ) = tp1 == tp2 && case liftA2 (==) n1 n2 of
    Just True -> True
    _ -> False
  (==) _ _ = False

instance ( Show a, Show b ) => Show ( AbsBinding a b ) where
  show ( TailAbs t ) = "TailAbs " ++ show t
  show ( ActiveAbs _ tp ( Just n ) ) = "ActiveAbs " ++ n ++ ":" ++ show tp
  show _ = "some nameless function"

instance Show TypeSimple where
  show ( TpsVar s ) = s
  show ( TpsArrow t1 t2 ) = "(" ++ show t1 ++ "->" ++ show t2 ++ ")"
  show ( TpsPoly s t ) = "@" ++ s ++ "." ++ show t

instance Show TermSimple where
  show ( TmsVar ( BoundVar s ) ) = s
  show ( TmsVar ( DataVar d ) ) = show d
  show ( TmsType t ) = show t
  show ( TmsAbs s tp tm ) = "(\\" ++ s ++ ":" ++ show tp ++ "." ++ show tm ++ ")"
  show ( TmsApp t1 t2 ) = show t1 ++ " " ++ show t2
  show ( TmsLet s t1 t2 ) = "let " ++ s ++ " = " ++ show t1 ++ " in " ++ show t2
  show ( TmsPoly s t ) = "(\\" ++ s ++ ":*." ++ show t ++ ")"

instance Show Context where
  show ( Context [] ) = "∅"
  show ( Context ( CmVar s t : other ) ) = s ++ ":" ++ show ( typeToTypeSimple ( Context other ) t ) ++ ", " ++ show ( Context other )
  show ( Context ( CmType s : other ) ) = s ++ ":*, " ++ show ( Context other )



class ShowWithContext a where
  showWithContext :: Context -> a -> String

instance ShowWithContext TermSimple where
  showWithContext _ = show

instance ShowWithContext Term where
  showWithContext ctx t = maybe
    "Could not convert term to term simple!"
    show 
      $ termToTermSimple ctx t

instance ShowWithContext TypeSimple where
  showWithContext _ = show

instance ShowWithContext Type where
  showWithContext ctx t = maybe
    "Could not convert term to term simple!"
    show 
      $ typeToTypeSimple ctx t