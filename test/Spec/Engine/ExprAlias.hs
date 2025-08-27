{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module Spec.Engine.ExprAlias (tests) where

import ControlledFixpoint.Engine as Engine
import ControlledFixpoint.Grammar as Grammar
import qualified Data.Map as Map
import Spec.Engine.Common
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "ExprAlias"
    [ex1_tests]

ex1_tests :: TestTree
ex1_tests =
  testGroup
    "ex1"
    [ mkTest_Engine
        "preserve alias"
        cfg
          { goals = [mkGoal 0 $ P (Swap (Tuple A B))],
            rules =
              [ (mkRule "R1")
                  []
                  (P "x")
              ]
          }
        ( EngineSuccessWithSubst . Subst . Map.fromList $
            [ ("x" {indexVar = Just 0}, Swap (Tuple A B))
            ]
        ),
      mkTest_Engine
        "unfold alias"
        cfg
          { goals = [mkGoal 0 $ P (Swap (Tuple A B))],
            rules =
              [ (mkRule "R1")
                  []
                  (P (Tuple "x" "y"))
              ]
          }
        ( EngineSuccessWithSubst . Subst . Map.fromList $
            [ ("x" {indexVar = Just 0}, B),
              ("y" {indexVar = Just 1}, A)
            ]
        )
    ]
  where
    cfg =
      Engine.Config
        { initialGas = FiniteGas 50,
          rules = [],
          goals = [],
          shouldSuspend = const False,
          exprAliases =
            [ ExprAlias \case
                Swap (Tuple x y) -> Just $ Tuple y x
                _ -> Nothing
            ],
          strategy = DepthFirstStrategy defaultDepthFirstStrategyOpts
        }

pattern A :: Expr C V
pattern A = "A" :% []

pattern B :: Expr C V
pattern B = "B" :% []

pattern P :: Expr C V -> Atom A C V
pattern P x = Atom "P" [x]

pattern Tuple :: Expr C V -> Expr C V -> Expr C V
pattern Tuple x y = "tuple" :% [x, y]

pattern Swap :: Expr C V -> Expr C V
pattern Swap t = "App" :% ["swap" :% [], t]
