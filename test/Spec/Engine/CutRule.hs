{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Engine.CutRule (tests) where

import Chronolog.Engine
import Chronolog.Grammar
import Data.Function ((&))
import Spec.Engine.Common
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "CutRule"
    [ mkTest_Engine
        "cut_lacking"
        ( (defaultConfig @String @String @String)
            { initialGas = FiniteGas 50,
              rules =
                [ mkRule
                    "R1"
                    []
                    (Atom "P" []),
                  mkRule
                    "R2"
                    [Atom "P" [] & GoalHyp . mkHypGoal]
                    (Atom "P" [])
                ],
              goals = [Atom "P" [] & mkGoal 0]
            }
        )
        (EngineError OutOfGas),
      mkTest_Engine
        "cut"
        ( (defaultConfig @String @String @String)
            { initialGas = FiniteGas 50,
              rules =
                [ ( mkRule
                      "R1"
                      []
                      (Atom "P" [])
                  )
                    { ruleOpts = defaultRuleOpts {cutRuleOpt = True}
                    },
                  mkRule
                    "R2"
                    [Atom "P" [] & GoalHyp . mkHypGoal]
                    (Atom "P" [])
                ],
              goals = [Atom "P" [] & mkGoal 0]
            }
        )
        EngineSuccess
    ]
