{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Chronolog.Library.AugmentGoalTrace where

import Control.Lens (FunctorWithIndex (imap))
import qualified Chronolog.Engine as Engine
import Chronolog.Grammar
import Data.Function
import Data.Functor ((<&>))
import qualified Data.List.Safe as List
import Data.String (IsString (..))
import Text.PrettyPrint (render, vcat, (<+>))
import Text.PrettyPrint.HughesPJClass (Pretty (..))
import Utility (bullets)

data Config = Config {}

-- | `augmentGoalTrace` is an `Engine.Config` tranformation that augments each
-- atom with a **goal trace** as an additional argument. The `Engine.goals` are
-- each assigned a unique goal trace constant. Each rule is updated to propagate
-- the goal trace of the conclusion atom the the goal traces of the hypothis
-- atoms.
augmentGoalTrace :: (Pretty c, IsString a, IsString v) => Config -> Engine.Config c v a -> Engine.Config c v a
augmentGoalTrace cfg cfg_engine =
  cfg_engine
    { Engine.rules = cfg_engine.rules & fmap (augmentGoalTrace_Rule cfg),
      Engine.goals = cfg_engine.goals & imap (augmentGoalTrace_Goal cfg),
      Engine.shouldSuspend = \case
        g@(Goal {atom = Atom a es}) -> case List.init es of
          Nothing ->
            error . render . vcat $
              [ "BUG: in the `shouldSuspend` function created by `augmentGoalTrace`, an atom does not have at least one argument, which should have been inserted already since the last argument of an atom encodes the goal trace",
                bullets ["atom =" <+> pPrint a]
              ]
          Just es' -> cfg_engine.shouldSuspend g {atom = Atom a es'}
    }

augmentGoalTrace_Goal :: (IsString v) => Config -> Int -> Goal c v a -> Goal c v a
augmentGoalTrace_Goal _cfg i goal = goal {atom = augmentGoalTrace_Atom _cfg i goal.atom}

augmentGoalTrace_Atom :: (IsString v) => Config -> Int -> Atom c v a -> Atom c v a
augmentGoalTrace_Atom _cfg i (Atom a es) = Atom a (es <> [fromString ("goal#" <> show i) :% []])

augmentGoalTrace_Rule :: (IsString a) => Config -> Rule c v a -> Rule c v a
augmentGoalTrace_Rule _cfg rule =
  rule
    { hyps =
        rule.hyps <&> \case
          GoalHyp goal' -> GoalHyp goal' {atom = goal'.atom {args = goal'.atom.args <> [goal]}},
      conc = case rule.conc of
        Atom a es -> Atom a (es <> [goal])
    }
  where
    goal = mkVarExpr (fromString "?goal")
