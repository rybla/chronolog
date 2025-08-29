{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module Spec.Engine.ExistentialVariables (tests) where

import Chronolog.Engine
import Chronolog.Grammar
import Data.Function ((&))
import qualified Data.Set as Set
import Spec.Engine.Common
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "ExistentialVariables"
    [ (mkTest_Engine "example-1")
        ( (defaultConfig @String @Int @String)
            { initialGas = FiniteGas 50,
              rules =
                [ mkRule
                    "R1"
                    [ Atom "Q" [0 :% []] & GoalHyp . mkHypGoal,
                      Atom "Q" ["x"] & GoalHyp . mkHypGoal
                    ]
                    (Atom "P" ["x"]),
                  mkRule
                    "R2"
                    []
                    (Atom "Q" ["x"])
                ],
              goals = [Atom "P" [1 :% []] & mkGoal 0]
            }
        )
        EngineSuccess,
      (mkTest_Engine "counter-1")
        ( (defaultConfig @String @Int @String)
            { initialGas = FiniteGas 50,
              rules =
                [ -- R1 will be used on P(1) goal to yield subgoals Q(1) and Q(0), and setting x := 1
                  mkRule
                    "R1"
                    [ Atom "Q" [0 :% []] & GoalHyp . mkHypGoal,
                      Atom "Q" ["x"] & GoalHyp . mkHypGoal
                    ]
                    (Atom "P" ["x"]),
                  -- R2 will be able to solve Q(1) since x := 1, but can't solve Q(0), and so engine will fail
                  mkRule'
                    "R2"
                    []
                    (Atom "Q" ["x"])
                    (\opts -> opts {existentialVarsRuleOpt = Set.fromList ["x"]})
                ],
              goals = [Atom "P" [1 :% []] & mkGoal 0]
            }
        )
        EngineFailure
    ]
