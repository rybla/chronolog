{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Engine.SuspendAndResume (tests) where

import ControlledFixpoint.Engine as Engine
import ControlledFixpoint.Grammar
import Spec.Engine.Common
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "SuspendAndResume"
    [ mkTest_Engine
        "simple suspend"
        Engine.Config
          { initialGas = FiniteGas 50,
            strategy = DepthFirstStrategy defaultDepthFirstStrategyOpts,
            rules =
              [ (mkRule "R1")
                  []
                  (A :~ B),
                (mkRule "R1")
                  []
                  (B :~ A)
              ],
            exprAliases = [],
            shouldSuspend = \case
              Goal{atom=VarExpr _ :~ VarExpr _} -> True
              _ -> False,
            goals =
              [ mkGoal 0 $ "y" :~ "x",
                mkGoal 1 $ A :~ "y",
                mkGoal 2 $ "x" :~ B
              ]
          }
        EngineSuccess,
      mkTest_Engine
        "simple nonterminating"
        Engine.Config
          { initialGas = FiniteGas 10,
            strategy = DepthFirstStrategy defaultDepthFirstStrategyOpts,
            rules =
              [ (mkRule "R1")
                  [GoalHyp . mkHypGoal $ A :~ S "x"]
                  (A :~ "x")
              ],
            exprAliases = [],
            shouldSuspend = const False,
            goals = [mkGoal 0 $ A :~ B]
          }
        (EngineError OutOfGas),
      unrolling_tests
    ]

unrolling_tests :: TestTree
unrolling_tests =
  testGroup
    "unrolling"
    [ mkTest_Engine
        "avoid nonterminating branch"
        cfg
          { goals = [mkGoal 0 $ P "x" "y", mkGoal 1 $ Q "y" B],
            shouldSuspend = \case
              Goal{atom=P (VarExpr _) (VarExpr _)} -> True
              _ -> False
          }
        EngineSuccess,
      mkTest_Engine
        "exhaust terminating branch"
        cfg
          { goals = [mkGoal 0 $ P "x" "y", mkGoal 1 $ Q "y" B]
          }
        (EngineError OutOfGas)
    ]
  where
    cfg :: Config A C V
    cfg =
      Engine.Config
        { initialGas = FiniteGas 10,
          strategy = DepthFirstStrategy defaultDepthFirstStrategyOpts,
          rules = rules1,
          exprAliases = [],
          shouldSuspend = const False,
          goals = []
        }

    rules1 :: [Rule A C V]
    rules1 =
      [ (mkRule "R1")
          [GoalHyp . mkHypGoal $ P (S "x") A]
          (P "x" A),
        (mkRule "R2")
          []
          (P B B),
        (mkRule "R3")
          []
          (Q B B)
      ]

pattern Valid :: Expr C V -> Atom A C V
pattern Valid st = Atom "Valid" [st]

pattern (:~) :: Expr C V -> Expr C V -> Atom A C V
pattern x :~ y = Valid (ConExpr (Con "Rel" [x, y]))

pattern P :: Expr C V -> Expr C V -> Atom A C V
pattern P x y = Valid (ConExpr (Con "P" [x, y]))

pattern Q :: Expr C V -> Expr C V -> Atom A C V
pattern Q x y = Valid (ConExpr (Con "Q" [x, y]))

pattern A :: Expr C V
pattern A = ConExpr (Con "A" [])

pattern B :: Expr C V
pattern B = ConExpr (Con "B" [])

pattern S :: Expr C V -> Expr C V
pattern S x = ConExpr (Con "S" [x])
