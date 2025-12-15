{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Engine.SolutionsCount (tests) where

import Chronolog.Engine as Engine
import Chronolog.Grammar
import Spec.Engine.Common
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "SolutionsCount"
    [ mkTest_Engine @A @C @V
        "v1"
        ( Engine.Config
            { initialGas = FiniteGas 50,
              strategy = DepthFirstStrategy defaultDepthFirstStrategyOpts,
              rules =
                [ (mkRule "P 1")
                    []
                    (Atom "P" [ConExpr (Con "1" [])]),
                  (mkRule "P 2")
                    []
                    (Atom "P" [ConExpr (Con "2" [])])
                ],
              exprAliases = [],
              shouldSuspend = const False,
              goals = [mkGoal 0 $ Atom "P" ["x"]],
              useIndexing = True
            }
        )
        (EngineSuccessWithSolutionsCount 2)
    ]
