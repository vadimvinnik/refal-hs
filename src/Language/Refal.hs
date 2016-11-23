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
    Term,
    ObjectTerm,
    PatternTerm,
    ActiveTerm,
    Expr,
    ObjectExpr,
    PatternExpr,
    ActiveExpr,
    MatchState,
    FuncDef,

    -- * Expression conversions
    objectToPatternTerm,
    patternToActiveTerm,
    objectToActiveTerm,
    objectToPatternExpr,
    patternToActiveExpr,
    objectToActiveExpr,

    -- * Semantics
    subst,
    eval
) where

import Data.String.ToString
import Data.Map as M hiding (map)
import Data.List as L hiding (map, insert)

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

data MatchState v a = MatchState {
  atoms :: Map v a,
  terms :: Map v (ObjectTerm a),
  exprs :: Map v (ObjectExpr a)
}

newtype FuncDef f a = FuncDef {
  getFuncDef :: Map f (ObjectExpr a -> ObjectExpr a)
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

objectToPatternTerm :: ObjectTerm a -> PatternTerm v a
objectToPatternTerm = fmap ObjectItem

patternToActiveTerm :: PatternTerm v a -> ActiveTerm f v a
patternToActiveTerm = fmap PatternItem

objectToActiveTerm :: ObjectTerm a -> ActiveTerm f v a
objectToActiveTerm = fmap (PatternItem . ObjectItem)

objectToPatternExpr :: ObjectExpr a -> PatternExpr v a
objectToPatternExpr = fmap ObjectItem

patternToActiveExpr :: PatternExpr v a -> ActiveExpr f v a
patternToActiveExpr = fmap PatternItem

objectToActiveExpr :: ObjectExpr a -> ActiveExpr f v a
objectToActiveExpr = fmap (PatternItem . ObjectItem)

-- TODO: pass conversion parameters - string representations of
-- opening and closing brackets, variable prefixes
instance (ToString a) => ToString (Term a) where
  toString (Item a) = toString a
  toString (Block ts) = "(" ++ (toString $ fromExpr ts) ++ ")"

instance (ToString a) => ToString [a] where -- [a] = (Expr a)
  toString = concat . map toString

instance (ToString v, ToString a) => ToString (PatternItem v a) where
  toString (ObjectItem i) = toString i
  toString (AtomVar v) = "s." ++ (toString v)
  toString (TermVar v) = "t." ++ (toString v)
  toString (ExprVar v) = "e." ++ (toString v)

instance (ToString f, ToString v, ToString a) => ToString (ActiveItem f v a) where
  toString (PatternItem i) = toString i
  toString (FunctionCall f e) = "<" ++ (toString f) ++ " " ++ (toString $ fromExpr e) ++ ">"

-- TODO: Parsers for the 3 expression types

substPatternItem :: Ord v => MatchState v a -> PatternItem v a -> ObjectExpr a
substPatternItem _ (ObjectItem x) = pure x
substPatternItem m (AtomVar v) = Expr [Item $ (atoms m) ! v]
substPatternItem m (TermVar v) = Expr [(terms m) ! v]
substPatternItem m (ExprVar v) = (exprs m) ! v

subst :: Ord v => MatchState v a -> PatternExpr v a -> ObjectExpr a
subst m e = e >>= substPatternItem m

evalActiveItem :: (Ord v, Ord f) => FuncDef f a -> MatchState v a -> ActiveItem f v a -> ObjectExpr a
evalActiveItem d m (PatternItem p) = substPatternItem m p
evalActiveItem d m (FunctionCall f e) = ((getFuncDef d) ! f) (eval d m e)

eval :: (Ord v, Ord f) => FuncDef f a -> MatchState v a -> ActiveExpr f v a -> ObjectExpr a
eval d m e = e >>= evalActiveItem d m

emptyMatchState :: MatchState v a
emptyMatchState = MatchState {atoms = empty, terms = empty, exprs = empty}

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
    [(m { atoms = insert v y $ atoms m }, Expr ts)]
    (\x -> if x == y then [(m, Expr ts)] else [])
    (M.lookup v $ atoms m)
matchTerm m (Item (TermVar v)) (Expr (t:ts)) =
  maybe
    [(m { terms = insert v t $ terms m }, Expr ts)]
    (\x -> if x == t then [(m, Expr ts)] else [])
    (M.lookup v $ terms m)
matchTerm m (Item (ExprVar v)) (Expr ts) =
  maybe
    (map (\(p, q) -> (m { exprs = insert v (Expr p) $ exprs m }, Expr q)) $ allSplits ts)
    (\(Expr ps) -> maybe [] (\rs -> [(m, Expr rs)]) $ stripPrefix ps ts)
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

allSplits :: [a] -> [([a], [a])]
allSplits [] = [([], [])]
allSplits l@(x:xs) = ([], l) : (map (\(p, q) -> (x:p, q)) $ allSplits xs)
