{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Eta reduce" #-}

module Spec.Engine.RuleSpecificSuspend (tests) where

import Chronolog.Engine
import Chronolog.Grammar
import Data.Function ((&))
import Spec.Engine.Common
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "RuleSpecificSuspend"
    . concat
    $ [ let mkTest_ex1 testName suspendRuleOpt result =
              mkTest_Engine
                ("ex1_" ++ testName)
                ( (defaultConfig @String @String @String)
                    { initialGas = FiniteGas 50,
                      rules =
                        [ let r =
                                mkRule
                                  "R1"
                                  [Atom "P" ["A" :% []] & GoalHyp . mkHypGoal]
                                  (Atom "P" ["x"])
                           in r {ruleOpts = r.ruleOpts {suspendRuleOpt}},
                          mkRule
                            "R2"
                            []
                            (Atom "P" ["A" :% []])
                        ],
                      goals = [mkGoal 0 (Atom "P" ["y"])]
                    }
                )
                result
         in [ mkTest_ex1
                "with_suspend"
                ( Just \case
                    Goal {atom = Atom "P" ["A" :% _]} -> True
                    _ -> False
                )
                EngineSuccessWithSuspends,
              mkTest_ex1 "without_suspend" Nothing (EngineError OutOfGas)
            ]
      ]
