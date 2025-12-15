{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import qualified Spec.Engine.Add
import qualified Spec.Engine.ApplicativeFunctorSubtyping
import qualified Spec.Engine.CutRule
import qualified Spec.Engine.ExprAlias
import qualified Spec.Engine.Library.AugmentDerivation
import qualified Spec.Engine.Library.AugmentGoalTrace
import qualified Spec.Engine.ExistentialVariables
import qualified Spec.Engine.PathIndexing
import qualified Spec.Engine.Proof
import qualified Spec.Engine.PruneAtRequiredGoalFailure
import qualified Spec.Engine.RuleSpecificSuspend
import qualified Spec.Engine.SolutionsCount
import qualified Spec.Engine.Subtyping
import qualified Spec.Engine.SuspendAndResume
import qualified Spec.Unification
import System.Environment (setEnv)
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = do
  "TASTY_NUM_THREADS" `setEnv` "1"
  defaultMain tests

tests :: TestTree
tests =
  testGroup
    "tests"
    [ Spec.Unification.tests,
      Spec.Engine.Add.tests,
      Spec.Engine.Subtyping.tests,
      Spec.Engine.SuspendAndResume.tests,
      Spec.Engine.SolutionsCount.tests,
      Spec.Engine.Proof.tests,
      Spec.Engine.PathIndexing.tests,
      Spec.Engine.ApplicativeFunctorSubtyping.tests,
      Spec.Engine.Library.AugmentDerivation.tests,
      Spec.Engine.Library.AugmentGoalTrace.tests,
      Spec.Engine.ExprAlias.tests,
      Spec.Engine.PruneAtRequiredGoalFailure.tests,
      Spec.Engine.CutRule.tests,
      Spec.Engine.ExistentialVariables.tests,
      Spec.Engine.RuleSpecificSuspend.tests
    ]
