{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Chronolog.Indexing where

import Chronolog.Grammar as G
import Control.Monad.State (MonadState (put), evalState, get)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Text.PrettyPrint.HughesPJClass (brackets, doubleQuotes, hcat, hsep, nest, punctuate, render, text, vcat, (<+>), (<>))
import Prelude hiding ((<>))

-- Implementation of path indexing based on Chapter 26 of Volume 2 of the Handbook of Automated Reasoning

--------------------------------------------------------------------------------
-- Path Strings
--------------------------------------------------------------------------------

-- For the purposes of indexing, all variable instances are distinct,
-- i.e. we treat all terms as linear.
data PathStringPart a c v = Pos Int | At a | Co c | Va
  deriving (Show, Eq, Ord)

type PathString a c v = [PathStringPart a c v]

genPathStrings :: Rule a c v -> [PathString a c v]
genPathStrings (Rule _ _ c _) = atomToPathStrings c

atomToPathStrings :: Atom a c v -> [PathString a c v]
atomToPathStrings (Atom n es) = map (At n :) $ argsToPathStrings 0 es

argsToPathStrings :: Int -> [Expr c v] -> [PathString a c v]
argsToPathStrings pos (x : xs) = map (Pos pos :) (exprToPathStrings x) ++ argsToPathStrings (succ pos) xs
argsToPathStrings 0 [] = [[]]
argsToPathStrings _ [] = []

exprToPathStrings :: Expr c v -> [PathString a c v]
exprToPathStrings (ConExpr (Con c es)) = map (Co c :) (argsToPathStrings 0 es)
exprToPathStrings (VarExpr (Var _ _)) = [[Va]]

--------------------------------------------------------------------------------
-- OrderedRule
--------------------------------------------------------------------------------

newtype OrderedRule a c v = WrapOrderedRule (Rule a c v)

-- Not sure if name-based ordering is acceptable
instance Eq (OrderedRule a c v) where
  WrapOrderedRule r1 == WrapOrderedRule r2 = r1.name == r2.name

instance Ord (OrderedRule a c v) where
  WrapOrderedRule r1 <= WrapOrderedRule r2 = r1.name <= r2.name

unwrapOrderedRule :: OrderedRule a c v -> Rule a c v
unwrapOrderedRule (WrapOrderedRule r) = r

--------------------------------------------------------------------------------
-- TrieSet
--------------------------------------------------------------------------------

-- | Set of rules that a trie leaf corresponds to
type TrieSet a c v = Set.Set (OrderedRule a c v)

trieSetEmpty :: TrieSet a c v
trieSetEmpty = Set.empty

trieSetInsert :: TrieSet a c v -> Rule a c v -> TrieSet a c v
trieSetInsert set rule = Set.insert (WrapOrderedRule rule) set

trieSetSingleton :: Rule a c v -> TrieSet a c v
trieSetSingleton rule = Set.singleton (WrapOrderedRule rule)

-- A similar function is included in containers >= 0.8 as Set.intersections
setIntersections :: (Ord a) => [Set.Set a] -> Set.Set a
setIntersections [] = Set.empty
setIntersections l = foldr1 Set.intersection l

--------------------------------------------------------------------------------
-- Trie
--------------------------------------------------------------------------------

-- | Trie made up of maps from path string parts to children.
-- `Leaf` should only be mapped to by `Nothing`
data Trie a c v
  = Node (Map (Maybe (PathStringPart a c v)) (Trie a c v))
  | Leaf (TrieSet a c v)

emptyTrie :: Trie a c v
emptyTrie = Leaf trieSetEmpty

newTrie :: Rule a c v -> PathString a c v -> Trie a c v
newTrie rule [] = Node (Map.singleton Nothing (Leaf (trieSetSingleton rule)))
newTrie rule (part : rest) = Node (Map.singleton (Just part) (newTrie rule rest))

trieType :: Trie a c v -> String
trieType (Leaf _) = "leaf"
trieType (Node _) = "node"

--------------------------------------------------------------------------------
-- FastGoal
--------------------------------------------------------------------------------

-- | Goal preprocessed to allow faster (and cleaner) random access via path strings
data FastGoal a c v = FastGoal
  { atom :: a,
    exprs :: Vector.Vector (FastGoalExpr c)
  }

data FastGoalExpr c = GoalCon c (Vector.Vector (FastGoalExpr c)) | GoalVar

preProcessGoal :: [ExprAlias c v] -> Goal a c v -> FastGoal a c v
preProcessGoal aliases goal =
  let Atom an es = normAliasesInAtom aliases (G.atom goal)
      go =
        Vector.fromList
          . map
            \case
              ConExpr (Con con con_args) -> GoalCon con (go con_args)
              VarExpr (Var _ _) -> GoalVar
   in FastGoal an (go es)

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

trieInsert :: (Ord a, Ord c) => Rule a c v -> Trie a c v -> Trie a c v
trieInsert rule trie =
  let paths = genPathStrings rule
   in foldr (insertPathString rule) trie paths

insertPathString :: (Ord a, Ord c) => Rule a c v -> PathString a c v -> Trie a c v -> Trie a c v
insertPathString rule [] (Leaf compatible) = Leaf $ trieSetInsert compatible rule
insertPathString rule (part : rest) (Node children) = Node $
  case Map.lookup (Just part) children of
    Just t -> Map.insert (Just part) (insertPathString rule rest t) children
    Nothing -> Map.insert (Just part) (newTrie rule rest) children
insertPathString rule [] (Node children) =
  Node $
    Map.insertWith newLeaf Nothing (Leaf (trieSetSingleton rule)) children
  where
    newLeaf _ (Leaf s) = Leaf $ trieSetInsert s rule
    -- unreachable for well-formed tries: Nothing should never map to a Node
    newLeaf singleton (Node _) = singleton
-- Should only happen at initial Trie construction
insertPathString rule path@(_ : _) (Leaf _) = newTrie rule path

{-# SCC buildIndex #-}
buildIndex :: forall a c v s. (Ord a, Ord c, Foldable s) => [ExprAlias c v] -> s (Rule a c v) -> Trie a c v
buildIndex aliases = foldr go (Leaf trieSetEmpty)
  where
    go rule trie =
      let Rule _ _ c _ = rule
          normedConc = normAliasesInAtom aliases c
          normedRule = rule {conc = normedConc}
       in trieInsert normedRule trie

{-# SCC filterPathIndexing #-}
filterPathIndexing :: (Ord a, Ord c) => [ExprAlias c v] -> Goal a c v -> Trie a c v -> [Rule a c v]
filterPathIndexing aliases goal trie =
  let fastGoal = preProcessGoal aliases goal
      -- Steps 1-3 of fig. 10 on pg. 1881. Collects all descendant states
      retrieve (Leaf compatible) GoalVar = compatible
      retrieve (Node m) GoalVar =
        Set.unions $
          List.map
            ( \child -> case child of
                Node _ -> retrieve child GoalVar
                Leaf compat -> compat
            )
            (Map.elems m)
      retrieve (Node m) (GoalCon c v) =
        -- Steps 4-13
        let base = case Map.lookup (Just $ Co c) m of
              Nothing -> Set.empty
              Just (Leaf _) -> Set.empty -- unreachable for well-formed tries
              Just (Node child) ->
                -- step 11
                let inters =
                      setIntersections $
                        Data.Maybe.mapMaybe
                          ( \(p, si) -> case p of
                              Nothing -> Nothing
                              Just (Pos i) -> case v Vector.!? i of
                                Nothing -> Nothing
                                Just g -> Just $ retrieve si g
                              Just _ -> Nothing
                          )
                          (Map.toList child)
                 in case Map.lookup Nothing child of
                      Just (Leaf compatible) -> if Vector.length v == 0 then compatible else inters
                      -- unreachable for well-formed tries
                      Just _ -> Set.empty
                      -- Not a final state
                      Nothing -> inters
            -- Steps 14-15
            varM = case Map.lookup (Just Va) m of
              Nothing -> Set.empty
              Just (Leaf compat) -> compat
              Just (Node child) -> case Map.lookup Nothing child of
                Nothing -> Set.empty
                Just (Leaf compat) -> compat
                Just (Node _) -> Set.empty
         in Set.union base varM
      retrieve (Leaf _) (GoalCon _ _) = Set.empty
   in case trie of
        Leaf _ -> []
        -- Get the right atom
        Node m -> case Map.lookup (Just $ At fastGoal.atom) m of
          Nothing -> []
          -- unreachable for well-formed tries
          Just (Leaf _) -> []
          Just (Node children) ->
            map unwrapOrderedRule $
              Set.toList $
                if Vector.length fastGoal.exprs == 0
                  then case Map.lookup Nothing children of
                    Just (Leaf compatible) -> compatible
                    _ -> Set.empty
                  else
                    setIntersections $
                      List.map
                        ( \i -> case Map.lookup (Just $ Pos i) children of
                            Just n@(Node _) -> retrieve n (fastGoal.exprs Vector.! i)
                            Nothing -> Set.empty
                            -- unreachable for well-formed tries
                            Just (Leaf _) -> Set.empty
                        )
                        [0 .. (Vector.length fastGoal.exprs - 1)]

-- | Generates a tree representation to be rendered by Graphviz dot
genGraphviz :: (Show a, Show c) => Trie a c v -> String
genGraphviz trie =
  let getNext = do
        i <- get
        put (succ i)
        return i
      getRels (Leaf compat) = do
        i <- getNext
        let showOrderedRule :: OrderedRule a c v -> String
            showOrderedRule = unRuleName . (\r -> r.name) . unwrapOrderedRule

            escapeQuotes "" = ""
            escapeQuotes ('\"' : cs) = "\\\"" ++ escapeQuotes cs
            escapeQuotes (c : cs) = c : escapeQuotes cs
            label = hcat $ punctuate (text ", ") $ map (text . escapeQuotes . showOrderedRule) $ Set.toList compat

            leafNode =
              hsep
                [ text . show $ i,
                  brackets . hcat . punctuate (text ",") $
                    [ text "label=" <> doubleQuotes label,
                      text "peripheries=2"
                    ]
                ]
                <> text ";"
        return ([leafNode], [])
      getRels (Node m) = do
        parent <- getNext
        let node = (text . show $ parent) <> text ";"
        mt <-
          mapM
            ( \(p, r) -> do
                i <- get
                (childNodes, childTrans) <- getRels r
                let tr = case p of
                      Nothing -> ((text . show) parent <+> text "->" <+> (text . show) i) <> text ";"
                      Just part ->
                        hsep
                          [ text . show $ parent,
                            text "->",
                            text . show $ i,
                            brackets . hcat . punctuate (text ",") $
                              [text "label=" <> doubleQuotes (text (filter (/= '"') $ show part))]
                          ]
                          <> text ";"
                return (childNodes, tr : childTrans)
            )
            (Map.toList m)
        return $ foldr (\(ns, trs) (accNodes, accTrs) -> (accNodes ++ ns, accTrs ++ trs)) ([node], []) mt
      (nodes, rels) = evalState (getRels trie) (0 :: Integer)
   in render . vcat $
        [ text "digraph Trie {",
          nest
            4
            ( vcat
                (nodes ++ rels)
            ),
          text "}"
        ]
