{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Chronolog.Freshening where

import Control.Monad.State (MonadState (..), State, modify)
import Chronolog.Grammar
import Data.Function ((&))
import Data.Set (Set)
import qualified Data.Set as Set
import Utility

data Env c v = Env
  { sigma :: Subst c v,
    freshCounter_vars :: Int,
    freshCounter_goals :: Int,
    existentialVars :: Set v
  }

type M c v = State (Env c v)

freshenRule :: (Ord v) => Rule a c v -> M c v (Rule a c v)
freshenRule rule = do
  modify \env -> env {existentialVars = rule.ruleOpts.existentialVarsRuleOpt <> env.existentialVars}
  hyps' <- rule.hyps <&>>= freshenHyp
  conc' <- rule.conc & freshenAtom
  return
    rule
      { hyps = hyps',
        conc = conc'
      }

freshenHyp :: (Ord v) => Hyp a c v -> M c v (Hyp a c v)
freshenHyp (GoalHyp goal) = GoalHyp <$> freshenGoal goal

-- | Only freshens the goal's `indexVar`.
freshenGoalIndex :: Goal a c v -> M c v (Goal a c v)
freshenGoalIndex goal = do
  goalIndex' <- do
    env <- get
    modify \env' -> env' {freshCounter_goals = env'.freshCounter_goals + 1}
    return $ Just env.freshCounter_goals
  return goal {goalIndex = goalIndex'}

freshenGoal :: (Ord v) => Goal a c v -> M c v (Goal a c v)
freshenGoal goal = do
  goal' <- freshenGoalIndex goal
  atom' <- freshenAtom goal'.atom
  return goal' {atom = atom'}

freshenAtom :: (Ord v) => Atom a c v -> M c v (Atom a c v)
freshenAtom (Atom a es) = Atom a <$> (es <&>>= freshenExpr)

freshenExpr :: (Ord v) => Expr c v -> M c v (Expr c v)
freshenExpr (VarExpr x) = freshenVar x
freshenExpr (ConExpr (Con c es)) = ConExpr . Con c <$> (es <&>>= freshenExpr)

freshenVar :: (Ord v) => Var v -> M c v (Expr c v)
freshenVar x = do
  env <- get
  if x.labelVar `Set.member` env.existentialVars
    then return $ VarExpr x
    else case x & substVar env.sigma of
      Just x' -> return x'
      Nothing -> do
        let freshCounter_vars' = env.freshCounter_vars + 1
            x' = VarExpr x {indexVar = Just env.freshCounter_vars}
        put
          env
            { sigma = env.sigma & setVar x x',
              freshCounter_vars = freshCounter_vars'
            }
        return x'
