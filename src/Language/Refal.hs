--
-- Vadim Vinnik, 2016-17
-- vadim.vinnik@gmail.com
--

{- |
Refal (acronym for REcursive Functional ALgorithmic language) is a specialised
language for symbol manipulation (string processing, parsing, translation, text
generation etc.) based on pattern matching, designed in the Soviet Union by
V.F.Turchin in 1960's.

This library provides a semantical engine for executing programs in generalised
Refal where data need not to be ASCII strings but can be combined from any kind
of discrete objects, the same holds for variable and function names.
-}

module Language.Refal (
    -- * Types
    ObjectItem(..),
    PatternItem(..),
    ActiveItem(..),
    Term(..),
    ObjectTerm(..),
    PatternTerm(..),
    ActiveTerm(..),
    Expr(..),
    ObjectExpr(..),
    PatternExpr(..),
    ActiveExpr(..),
    Sentence(..),
    FunctionBody(..),
    Module(..),
    MatchState(..),
    Semantics(..),

    -- * smart constructors
    empty,
    quote,
    svar,
    tvar,
    evar,
    call,
    block,
    (.+.),

    -- * Expression conversions
    ot2p,
    pt2a,
    ot2a,
    oe2p,
    pe2a,
    oe2a,

    -- * Semantics
    subst,
    eval,

    -- * Engine
    match,
    applySentence,
    applyFunctionBody,
    interpretModule,
    evalFunctionCall
) where

import Data.String.ToString
import Data.Map ((!))
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe (listToMaybe, fromJust)
import Control.Monad (msum)

import Utils.CharToString
import Utils.AllSplits

newtype Expr a = Expr { fromExpr :: [Term a] }
  deriving (Show, Eq)

data Term a
  = Item a
  | Block (Expr a)
  deriving (Show, Eq)

type ObjectExpr a = Expr (ObjectItem a)
type PatternExpr v a = Expr (PatternItem v a)
type ActiveExpr f v a = Expr (ActiveItem f v a)

type ObjectTerm a = Term (ObjectItem a)
type PatternTerm v a = Term (PatternItem v a)
type ActiveTerm f v a = Term (ActiveItem f v a)

type ObjectItem a = a

data PatternItem v a
  = ObjectItem (ObjectItem a)
  | AtomVar v
  | TermVar v
  | ExprVar v
  deriving (Show, Eq)

data ActiveItem f v a
  = PatternItem (PatternItem v a)
  | FunctionCall f (ActiveExpr f v a)
  deriving (Show, Eq)

data Sentence f v a = Sentence {
  pattern  :: PatternExpr v a,
  template :: ActiveExpr f v a
}

newtype FunctionBody f v a = FunctionBody {
  sentences :: [Sentence f v a]
}

newtype Module f v a = Module {
  functions :: M.Map f (FunctionBody f v a)
}

data MatchState v a = MatchState {
  atoms :: M.Map v a,
  terms :: M.Map v (ObjectTerm a),
  exprs :: M.Map v (ObjectExpr a)
} deriving (Show)

newtype Semantics f a = Semantics {
  semantics :: M.Map f (ObjectExpr a -> ObjectExpr a)
}

instance Functor Term where
  fmap f (Item a) = Item (f a)
  fmap f (Block ts) = Block (fmap f ts)

instance Functor Expr where
  fmap f = Expr . map (fmap f) . fromExpr

instance Applicative Expr where
  pure = Expr . pure . Item
  ef <*> ex = ef >>= ((flip fmap) ex)

instance Monad Expr where
  e >>= f = Expr $ concat $ (map $ fromExpr . bindExprHelper) $ fromExpr e where
    bindExprHelper (Item x) = f x
    bindExprHelper (Block e1) = Expr [Block (e1 >>= f)]

empty :: Expr a
empty = Expr []

quote :: [a] -> ObjectExpr a
quote = Expr . map Item

svar :: v -> PatternExpr v a
svar v = Expr [Item $ AtomVar v]

tvar :: v -> PatternExpr v a
tvar v = Expr [Item $ TermVar v]

evar :: v -> PatternExpr v a
evar v = Expr [Item $ ExprVar v]

call :: f -> ActiveExpr f v a -> ActiveExpr f v a
call f e = Expr [Item $ FunctionCall f e]

block :: Expr a -> Expr a
block e = Expr [Block e]

(.+.) :: Expr a -> Expr a -> Expr a
(Expr e1) .+. (Expr e2) = Expr (e1 ++ e2)

ot2p :: ObjectTerm a -> PatternTerm v a
ot2p = fmap ObjectItem

pt2a :: PatternTerm v a -> ActiveTerm f v a
pt2a = fmap PatternItem

ot2a :: ObjectTerm a -> ActiveTerm f v a
ot2a = fmap (PatternItem . ObjectItem)

oe2p :: ObjectExpr a -> PatternExpr v a
oe2p = fmap ObjectItem

pe2a :: PatternExpr v a -> ActiveExpr f v a
pe2a = fmap PatternItem

oe2a :: ObjectExpr a -> ActiveExpr f v a
oe2a = fmap (PatternItem . ObjectItem)

-- TODO: pass conversion parameters - string representations of
-- opening and closing brackets, variable prefixes
instance (ToString a) => ToString (Term a) where
  toString (Item a) = toString a
  toString (Block ts) = "(" ++ (toString ts) ++ ")"

instance (ToString a) => ToString (Expr a) where
  toString = concat . map toString . fromExpr

instance (ToString v, ToString a) => ToString (PatternItem v a) where
  toString (ObjectItem i) = toString i
  toString (AtomVar v) = " s." ++ (toString v) ++ " "
  toString (TermVar v) = " t." ++ (toString v) ++ " "
  toString (ExprVar v) = " e." ++ (toString v) ++ " "

instance (ToString f, ToString v, ToString a) => ToString (ActiveItem f v a) where
  toString (PatternItem i) = toString i
  toString (FunctionCall f e) = " <" ++ (toString f) ++ " " ++ (toString e) ++ "> "

-- TODO: Parsers for the 3 expression types

substPatternItem :: Ord v => MatchState v a -> PatternItem v a -> ObjectExpr a
substPatternItem _ (ObjectItem x) = pure x
substPatternItem m (AtomVar v) = Expr [Item $ (atoms m) ! v]
substPatternItem m (TermVar v) = Expr [(terms m) ! v]
substPatternItem m (ExprVar v) = (exprs m) ! v

subst :: Ord v => MatchState v a -> PatternExpr v a -> ObjectExpr a
subst m e = e >>= substPatternItem m

evalActiveItem :: (Ord v, Ord f) => Semantics f a -> MatchState v a -> ActiveItem f v a -> ObjectExpr a
evalActiveItem d m (PatternItem p) = substPatternItem m p
evalActiveItem d m (FunctionCall f e) = ((semantics d) ! f) (eval d m e)

eval :: (Ord v, Ord f) => Semantics f a -> MatchState v a -> ActiveExpr f v a -> ObjectExpr a
eval d m e = e >>= evalActiveItem d m

emptyMatchState :: MatchState v a
emptyMatchState = MatchState {atoms = M.empty, terms = M.empty, exprs = M.empty}

matchTerm
  :: (Ord v, Eq a)
  => MatchState v a
  -> PatternTerm v a
  -> ObjectExpr a
  -> [(MatchState v a, ObjectExpr a)]
matchTerm m (Item (ObjectItem x)) (Expr ((Item y):ts))
  | x == y     = [(m, Expr ts)]
  | otherwise  = []
matchTerm m (Item (AtomVar v)) (Expr ((Item y):ts)) =
  maybe
    [(m { atoms = M.insert v y $ atoms m }, Expr ts)]
    (\x -> if x == y then [(m, Expr ts)] else [])
    (M.lookup v $ atoms m)
matchTerm m (Item (TermVar v)) (Expr (t:ts)) =
  maybe
    [(m { terms = M.insert v t $ terms m }, Expr ts)]
    (\x -> if x == t then [(m, Expr ts)] else [])
    (M.lookup v $ terms m)
matchTerm m (Item (ExprVar v)) (Expr ts) =
  maybe
    (map (\(p, q) -> (m { exprs = M.insert v (Expr p) $ exprs m }, Expr q)) $ allSplits ts)
    (\(Expr ps) -> maybe [] (\rs -> [(m, Expr rs)]) $ L.stripPrefix ps ts)
    (M.lookup v $ exprs m)
matchTerm m (Block p) (Expr ((Block e):ts)) =
  map (\n -> (n, Expr ts)) $ matchWithState m p e
matchTerm _ _ _ = []

matchWithState :: (Ord v, Eq a) => MatchState v a -> PatternExpr v a -> ObjectExpr a -> [MatchState v a]
matchWithState m (Expr []) (Expr []) = [m]
matchWithState m (Expr (p:ps)) e =
  (matchTerm m p e) >>= (\(n, r) -> matchWithState n (Expr ps) r)
matchWithState _ _ _ = []

match :: (Ord v, Eq a) => PatternExpr v a -> ObjectExpr a -> [MatchState v a]
match = matchWithState emptyMatchState

applySentence :: (Ord f, Ord v, Eq a) => Semantics f a -> Sentence f v a -> ObjectExpr a -> Maybe (ObjectExpr a)
applySentence d (Sentence p t) e = fmap (\m -> eval d m t) (listToMaybe $ match p e)

applyFunctionBody :: (Ord f, Ord v, Eq a) => Semantics f a -> FunctionBody f v a -> ObjectExpr a -> ObjectExpr a
applyFunctionBody d b e = fromJust $ msum $ map (\s -> applySentence d s e) $ sentences b

interpretModule :: (Ord f, Ord v, Eq a) => Module f v a -> Semantics f a
interpretModule (Module m) = d where -- fixed point
  d = Semantics $ M.empty `M.union` M.map (applyFunctionBody d) m

evalFunctionCall :: (Ord f, Ord v, Eq a) => Module f v a -> f -> ObjectExpr a -> ObjectExpr a
evalFunctionCall m f e = ((semantics $ interpretModule m) ! f) e

