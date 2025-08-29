{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Chronolog.Library.AugmentDerivation where

import Control.Lens (FunctorWithIndex (imap))
import qualified Chronolog.Engine as Engine
import Chronolog.Grammar
import Data.Bifunctor (second)
import Data.Coerce (coerce)
import Data.Function
import Data.Functor ((<&>))
import qualified Data.List.Safe as List
import Data.String (IsString (..))
import Text.PrettyPrint (render, vcat, (<+>))
import Text.PrettyPrint.HughesPJClass (Pretty (..))
import Utility (bullets)

data Config a = Config
  { isDerivation :: a -> Maybe String
  }

-- | `augmentDerivation` is an `Engine.Config` tranformation that promotes some
-- of its atoms as **derivations** (as defined in the `Config`). A derivation
-- atom gets an additional argument which corresponds to its **derivation
-- tree**. The derivation tree has a node for each time a rule was used, and the
-- children of that node are the derivation trees for each of the hypotheses of
-- that rule use.
--
-- The variable names of the new arguments are specified by the `isDerivation`
-- function, and also given a number corresponding to the index of the
-- hypothesis that the atom is used in.
--
-- The nodes of the derivation tree use new expression constructors that are
-- derived from the names of the rules they correspond to.
--
-- See the goldenfile tests for this module for examples of what this
-- augmentation looks like.
augmentDerivation :: (IsString v, IsString c, Pretty a, Pretty c, Pretty v) => Config a -> Engine.Config a c v -> Engine.Config a c v
augmentDerivation cfg cfg_engine =
  cfg_engine
    { Engine.rules = cfg_engine.rules <&> augmentDerivation_Rule cfg,
      Engine.goals = cfg_engine.goals & imap (augmentDerivation_Goal cfg),
      Engine.shouldSuspend = \case
        g@(Goal {atom = a@(Atom n es)}) | Just _ <- cfg.isDerivation n -> case List.init es of
          Nothing ->
            error . render . vcat $
              [ "BUG: in the `shouldSuspend` function created by `augmentDerivation`, a derivation atom does not have at least one argument, which should have been inserted already since the last argument of a derivation atom encodes the derivation itself",
                bullets ["atom =" <+> pPrint a]
              ]
          Just es' -> cfg_engine.shouldSuspend g {atom = Atom n es'}
        a -> cfg_engine.shouldSuspend a
    }

augmentDerivation_Goal :: (IsString v) => Config a -> Int -> Goal a c v -> Goal a c v
augmentDerivation_Goal cfg i goal = goal {atom = augmentDerivation_Atom cfg i goal.atom}

augmentDerivation_Atom :: (IsString v) => Config a -> Int -> Atom a c v -> Atom a c v
augmentDerivation_Atom cfg i (Atom a es)
  | Just s <- cfg.isDerivation a,
    x <- mkVarExpr (fromString (s <> show i)) =
      Atom a (es <> [x])
augmentDerivation_Atom _ _ a = a

augmentDerivation_Rule :: (IsString v, IsString c) => Config a -> Rule a c v -> Rule a c v
augmentDerivation_Rule cfg rule =
  let (hyps', hypDrvs) =
        rule.hyps
          & imap
            ( \i -> \case
                GoalHyp goal@(Goal {atom = Atom c es})
                  | Just s <- cfg.isDerivation c,
                    x <- mkVarExpr (fromString (s <> show i)) ->
                      (GoalHyp goal {atom = Atom c (es <> [x])}, [x])
                h -> (h, [])
            )
          & unzip
          & second concat
   in rule
        { hyps = hyps',
          conc = case rule.conc of
            Atom c es | Just _ <- cfg.isDerivation c -> Atom c (es <> [fromString (coerce rule.name) :% hypDrvs])
            a -> a
        }
