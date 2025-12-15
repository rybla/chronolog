{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use ++" #-}

module Chronolog.Grammar where

import Chronolog.Common.Msg (Msg)
import qualified Chronolog.Common.Msg as Msg
import Control.Applicative ((<|>))
import Control.Monad (unless)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Writer (MonadWriter, tell)
import Control.Newtype.Generics (Newtype, over)
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.List.Safe as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString (fromString))
import GHC.Generics (Generic)
import Text.PrettyPrint (braces, brackets, comma, hang, hcat, hsep, nest, parens, punctuate, text, vcat, (<+>))
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))
import Utility

--------------------------------------------------------------------------------
-- Rule
--------------------------------------------------------------------------------

-- | Rule
data Rule a c v = Rule
  { name :: RuleName,
    hyps :: [Hyp a c v],
    conc :: Atom a c v,
    ruleOpts :: RuleOpts a c v
  }

instance (Pretty a, Pretty c, Pretty v) => Pretty (Rule a c v) where
  pPrint rule =
    vcat
      [ "rule" <+> pPrint rule.name,
        nest 2 . vcat $
          [ vcat (rule.hyps <&> pPrint),
            "--------------------------------",
            pPrint rule.conc
          ]
      ]

mkRule :: RuleName -> [Hyp a c v] -> Atom a c v -> Rule a c v
mkRule name hyps conc =
  Rule
    { name,
      hyps,
      conc,
      ruleOpts = defaultRuleOpts
    }

mkRule' :: RuleName -> [Hyp a c v] -> Atom a c v -> (RuleOpts a c v -> RuleOpts a c v) -> Rule a c v
mkRule' name hyps conc f_ruleOpts =
  Rule
    { name,
      hyps,
      conc,
      ruleOpts = f_ruleOpts defaultRuleOpts
    }

data RuleOpts a c v = RuleOpts
  { cutRuleOpt :: Bool,
    suspendRuleOpt :: Maybe (Goal a c v -> Bool),
    existentialVarsRuleOpt :: Set v
  }

instance (Pretty v) => Pretty (RuleOpts a c v) where
  pPrint ruleOpts =
    braces . commas . concat $
      [ ["cut" | ruleOpts.cutRuleOpt],
        ["suspend" | isJust ruleOpts.suspendRuleOpt],
        ["existentialVars" <> parens (hsep . punctuate "," . fmap pPrint . Set.toList $ ruleOpts.existentialVarsRuleOpt) | not (Set.null ruleOpts.existentialVarsRuleOpt)]
      ]

defaultRuleOpts :: RuleOpts a c v
defaultRuleOpts =
  RuleOpts
    { cutRuleOpt = False,
      suspendRuleOpt = Nothing,
      existentialVarsRuleOpt = Set.empty
    }

-- | Hypothesis.
--
-- NOTE: For now, there is only one kind of hypothesis: `GoalHyp`. However, I
-- made this a data type anyway since it may be desirable at some point to have
-- other kinds of hypotheses, such as functional predicates (predicates that are
-- checked by executing a `Bool`-valued function.)
data Hyp a c v
  = GoalHyp (Goal a c v)
  deriving (Show, Eq)

instance (Pretty a, Pretty c, Pretty v) => Pretty (Hyp a c v) where
  pPrint (GoalHyp g) =
    hsep
      [ g.goalIndex & maybe mempty (("G#" <>) . pPrint),
        pPrint g.atom,
        pPrint g.goalOpts
      ]

--------------------------------------------------------------------------------
-- Goal
--------------------------------------------------------------------------------

-- | A `Goal` is an `Atom` along with any other goal-relevant options and metadata.
data Goal a c v = Goal
  { atom :: Atom a c v,
    goalOpts :: GoalOpts,
    goalIndex :: GoalIndex
  }
  deriving (Show, Eq)

instance (Pretty a, Pretty c, Pretty v) => Pretty (Goal a c v) where
  pPrint g =
    hsep
      [ g.goalIndex & maybe (brackets "X") (brackets . ("G#" <>) . pPrint),
        pPrint g.atom,
        pPrint g.goalOpts
      ]

type GoalIndex = Maybe Int

data GoalOpts = GoalOpts
  { requiredGoalOpt :: Bool,
    constrainedRulesetGoalOpt :: Maybe (Set RuleName)
  }
  deriving (Show, Eq)

instance Pretty GoalOpts where
  pPrint opts =
    braces . commas . concat $
      [ ["required" | opts.requiredGoalOpt],
        ["constrained ruleset:" <+> pPrint rns | rns <- foldMap (List.singleton . Set.toList) opts.constrainedRulesetGoalOpt]
      ]

defaultGoalOpts :: GoalOpts
defaultGoalOpts =
  GoalOpts
    { requiredGoalOpt = False,
      constrainedRulesetGoalOpt = Nothing
    }

mkGoal :: Int -> Atom a c v -> Goal a c v
mkGoal i atom =
  Goal
    { atom,
      goalIndex = Just i,
      goalOpts = defaultGoalOpts
    }

mkHypGoal :: Atom a c v -> Goal a c v
mkHypGoal atom =
  Goal
    { atom,
      goalIndex = Nothing,
      goalOpts = defaultGoalOpts
    }

--------------------------------------------------------------------------------
-- Atom
--------------------------------------------------------------------------------

-- | An 'Atom' is an atomic formula.
data Atom a c v = Atom {name :: a, args :: [Expr c v]}
  deriving (Show, Eq, Ord)

instance (Pretty a, Pretty c, Pretty v) => Pretty (Atom a c v) where
  pPrint (Atom c es) = pPrint c <+> (es <&> pPrint & hsep)

--------------------------------------------------------------------------------
-- Expr
--------------------------------------------------------------------------------

-- | Expression
data Expr c v
  = ConExpr (Con c v)
  | VarExpr (Var v)
  deriving (Show, Eq, Ord)

instance (Pretty c, Pretty v) => Pretty (Expr c v) where
  pPrint (VarExpr x) = pPrint x
  pPrint (ConExpr c) = pPrint c

instance (IsString v) => IsString (Expr c v) where fromString x = VarExpr (fromString x)

--------------------------------------------------------------------------------
-- Var
--------------------------------------------------------------------------------

-- | Meta-variable that can be substituted with an expression. `indexVar` must
-- be provided manually for variables outside `Rule`s. `indexVar` will be
-- freshened automatically for variables inside `Rule`s.
data Var v = Var
  { labelVar :: v,
    indexVar :: Maybe Int
  }
  deriving (Show, Eq, Ord)

instance (IsString v) => IsString (Var v) where
  fromString s =
    Var
      { labelVar = fromString s,
        indexVar = Nothing
      }

instance (Pretty v) => Pretty (Var v) where
  pPrint x =
    hcat
      [ pPrint x.labelVar,
        x.indexVar & maybe mempty (text . subscriptNumber)
      ]

mkVarExpr :: Var v -> Expr c v
mkVarExpr = VarExpr

--------------------------------------------------------------------------------
-- Con
--------------------------------------------------------------------------------

-- | Constructor expression
data Con c v = Con {name :: c, args :: [Expr c v]}
  deriving (Show, Eq, Ord)

instance (Pretty c, Pretty v) => Pretty (Con c v) where
  pPrint (Con c []) = pPrint c
  pPrint (Con c es) = parens $ pPrint c <+> (es <&> pPrint & punctuate " " & hcat)

instance (IsString c) => IsString (Con c v) where
  fromString s = Con (fromString s) []

pattern (:%) :: c -> [Expr c v] -> Expr c v
pattern c :% es = ConExpr (Con c es)

infix 4 :%

mkConExpr :: c -> [Expr c v] -> Expr c v
mkConExpr c es = c :% es

--------------------------------------------------------------------------------
-- Subst
--------------------------------------------------------------------------------

-- | Substitution of meta-variables
newtype Subst c v = Subst (Map (Var v) (Expr c v))
  deriving (Show, Eq, Generic)

instance Newtype (Subst c v)

instance (Pretty c, Pretty v) => Pretty (Subst c v) where
  pPrint (Subst m) =
    if vertical
      then
        hang "substitution:" 4 . bullets $
          m
            & Map.toList
            & fmap \(x, e) -> pPrint x <+> ":=" <+> pPrint e
      else
        braces $
          m
            & Map.toList
            <&> (\(x, e) -> pPrint x <+> ":=" <+> pPrint e)
            & punctuate comma
            & hsep
    where
      vertical = False

unSubst :: Subst c v -> Map (Var v) (Expr c v)
unSubst (Subst m) = m

--------------------------------------------------------------------------------
-- ExprAlias
--------------------------------------------------------------------------------

-- | An expression alias. These are unfolded lazily during unification, so as a
-- result the solver does it's best to not unfold definitions unless it's
-- required in order to unify two expressions. Note that you _do_ need to be
-- careful about recursively aliases, since a recursive alias could lead to
-- infinite unfolding during unification.
newtype ExprAlias c v = ExprAlias (Expr c v -> Maybe (Expr c v))

unExprAlias :: ExprAlias c v -> (Expr c v -> Maybe (Expr c v))
unExprAlias (ExprAlias f) = f

applyExprAlias :: [ExprAlias c v] -> Expr c v -> Maybe (Expr c v)
applyExprAlias aliases e = foldr (\(ExprAlias f) -> (f e <|>)) Nothing aliases

normAliasesInGoal :: (MonadWriter [Msg] m) => [ExprAlias c v] -> Goal a c v -> m (Goal a c v)
normAliasesInGoal exprAliases goal = do
  let atom = normAliasesInAtom exprAliases goal.atom
  return goal {atom}

normAliasesInAtom :: [ExprAlias c v] -> Atom a c v -> Atom a c v
normAliasesInAtom exprAliases (Atom a es) = Atom a $ map (normAliasesInExpr exprAliases) es

normAliasesInExpr :: forall c v. [ExprAlias c v] -> Expr c v -> Expr c v
normAliasesInExpr exprAliases e0 = do
  let -- unfolds an alias in the left-most position, if any
      go :: Expr c v -> Maybe (Expr c v)
      go e = do
        case exprAliases `applyExprAlias` e of
          Just e' -> Just e'
          Nothing -> case e of
            VarExpr _ -> Nothing
            ConExpr (Con c esOrignal) -> ConExpr . Con c <$> go' [] esOrignal
              where
                go' _ [] = Nothing
                go' es_before (e_i : es_after) = (mappend es_before <$> ((:) <$> go e_i <*> return es_after)) <|> go' (es_before <> [e_i]) es_after
  case go e0 of
    Nothing -> e0
    Just e' -> normAliasesInExpr exprAliases e'

normHeadAliasesInExpr :: forall c v m. (MonadWriter [Msg] m, Pretty c, Pretty v) => [ExprAlias c v] -> Expr c v -> m (Expr c v)
normHeadAliasesInExpr exprAliases e = do
  case exprAliases `applyExprAlias` e of
    Nothing -> return e
    Just e' -> do
      tell
        [ (Msg.mk 3 "unfolded alias")
            { Msg.contents =
                [ "before :" <+> pPrint e,
                  "after  :" <+> pPrint e'
                ]
            }
        ]
      normHeadAliasesInExpr exprAliases e'

--------------------------------------------------------------------------------
-- Names
--------------------------------------------------------------------------------

-- | Rule name
newtype RuleName = RuleName String
  deriving (Show, Eq, Ord)

unRuleName :: RuleName -> String
unRuleName (RuleName s) = s

instance IsString RuleName where fromString = RuleName

instance Pretty RuleName where pPrint (RuleName x) = text x

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

varsAtom :: (Ord v) => Atom a c v -> Set (Var v)
varsAtom (Atom _ es) = es <&> varsExpr & Set.unions

varsExpr :: (Ord v) => Expr c v -> Set (Var v)
varsExpr (VarExpr x) = Set.singleton x
varsExpr (ConExpr (Con _ es)) = es <&> varsExpr & Set.unions

occursInAtom :: (Ord v) => Var v -> Atom a c v -> Bool
occursInAtom x a = x `Set.member` varsAtom a

occursInExpr :: (Ord v) => Var v -> Expr c v -> Bool
occursInExpr x e = x `Set.member` varsExpr e

emptySubst :: Subst c v
emptySubst = Subst Map.empty

setVar :: (Ord v) => Var v -> Expr c v -> Subst c v -> Subst c v
setVar x e =
  over Subst $
    fmap (substExpr (Subst (Map.singleton x e)))
      . Map.insert x e

substRule :: (Ord v) => Subst c v -> Rule a c v -> Rule a c v
substRule sigma rule =
  Rule
    { name = rule.name,
      hyps = rule.hyps <&> substHyp sigma,
      conc = rule.conc & substAtom sigma,
      ruleOpts = rule.ruleOpts
    }

substHyp :: (Ord v) => Subst c v -> Hyp a c v -> Hyp a c v
substHyp sigma (GoalHyp g) = GoalHyp (substGoal sigma g)

substGoal :: (Ord v) => Subst c v -> Goal a c v -> Goal a c v
substGoal sigma goal = goal {atom = substAtom sigma goal.atom}

substAtom :: (Ord v) => Subst c v -> Atom a c v -> Atom a c v
substAtom sigma (Atom c es) = Atom c (es <&> substExpr sigma)

substExpr :: (Ord v) => Subst c v -> Expr c v -> Expr c v
substExpr (Subst m) (VarExpr x) = case m Map.!? x of
  Nothing -> VarExpr x
  Just e -> e
substExpr sigma (ConExpr (Con c es)) =
  ConExpr (Con c (es <&> substExpr sigma))

substVar :: (Ord v) => Subst c v -> Var v -> Maybe (Expr c v)
substVar (Subst m) x = m Map.!? x

-- | Throws an error if 'sigma'' substitutes a variable that is also substituted
-- by 'sigma'.
composeSubst :: (MonadError Msg m, Ord v, Pretty v, Pretty c) => Subst c v -> Subst c v -> m (Subst c v)
composeSubst sigma@(Subst m) sigma'@(Subst m') = do
  let keysIntersection = (m & Map.keysSet) `Set.intersection` (m' & Map.keysSet)
  unless (Set.null keysIntersection) do
    throwError $
      (Msg.mk 0 "In 'composeSubst', there are variables that both substitutions substitute")
        { Msg.contents =
            [ "keysIntersection =" <+> (keysIntersection & Set.toList & pPrint),
              "sigma  =" <+> pPrint sigma,
              "sigma' =" <+> pPrint sigma'
            ]
        }
  return $ Subst $ m `Map.union` (m' <&> substExpr sigma)

-- | Similar to `composeSubst`, but doesn't check for overlaps.
composeSubst_unsafe :: (Ord v) => Subst c v -> Subst c v -> Subst c v
composeSubst_unsafe sigma'@(Subst m') sigma@(Subst m) = Subst $ (m' <&> substExpr sigma) `Map.union` (m <&> substExpr sigma')
