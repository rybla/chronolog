{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module Spec.Engine.PruneAtRequiredGoalFailure (tests) where

import qualified Chronolog.Engine as Engine
import Chronolog.Grammar
import Spec.Engine.Common (mkTest_Engine)
import qualified Spec.Engine.Common as Common
import Test.Tasty (TestTree, testGroup)
import Text.PrettyPrint.HughesPJClass (Pretty (..), text)

data A = P | Q | R deriving (Show, Eq, Ord)

instance Pretty A where pPrint = text . show

data C = A | B deriving (Show, Eq, Ord)

instance Pretty C where pPrint = text . show

type V = String

tests :: TestTree
tests =
  testGroup
    "PruneAtRequiredGoalFailure"
    [ -- Due to the `requiredGoalOpt = True` on the first hypothesis of `Rule1`, it will not infinitely loop because the first use of `Rule1` will fail to solve that hypothesis which will prune that branch. And so the result will be an `EngineFailure` (`failedGoals` is populated) rather that an `EngineError` (due to running out of gas)
      mkTest_Engine @A @C @V
        "ex1 with required"
        Engine.Config
          { initialGas = Engine.FiniteGas 5,
            rules =
              [ (mkRule "Rule1")
                  [ GoalHyp (mkHypGoal (Atom Q [])) {goalOpts = defaultGoalOpts {requiredGoalOpt = True}},
                    GoalHyp (mkHypGoal (Atom P []))
                  ]
                  (Atom P [])
              ],
            goals =
              [ mkGoal 0 $ Atom P []
              ],
            shouldSuspend = const False,
            exprAliases = [],
            strategy = Engine.DepthFirstStrategy Engine.defaultDepthFirstStrategyOpts
          }
        Common.EngineFailure,
      mkTest_Engine @A @C @V
        "ex1 without required"
        Engine.Config
          { initialGas = Engine.FiniteGas 5,
            rules =
              [ (mkRule "Rule1")
                  [ GoalHyp (mkHypGoal (Atom Q [])),
                    GoalHyp (mkHypGoal (Atom P []))
                  ]
                  (Atom P [])
              ],
            goals =
              [ mkGoal 0 $ Atom P []
              ],
            shouldSuspend = const False,
            exprAliases = [],
            strategy = Engine.DepthFirstStrategy Engine.defaultDepthFirstStrategyOpts
          }
        (Common.EngineError Engine.OutOfGas)
    ]
