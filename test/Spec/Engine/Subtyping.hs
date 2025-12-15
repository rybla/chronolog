{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Engine.Subtyping (tests) where

import Chronolog.Engine as Engine
import Chronolog.Grammar
import Spec.Engine.Common
import Test.Tasty (TestTree, testGroup)
import Text.PrettyPrint.HughesPJClass (prettyShow)

tests :: TestTree
tests =
  testGroup
    "Subtyping"
    [ mkTest
        (int `arr` bool)
        (nat `arr` bool)
        EngineSuccess,
      mkTest
        (nat `arr` bool)
        (int `arr` bool)
        EngineFailure,
      mkTest
        "x"
        ("y" `arr` "z")
        EngineSuccessWithSuspends,
      mkTest
        ("x" `arr` "y")
        ("z" `arr` "w")
        EngineSuccessWithSuspends
    ]

mkTest :: Expr C V -> Expr C V -> EngineResult C V -> TestTree
mkTest a b =
  mkTest_Engine
    ("`" <> displayExpr a <> "  <:  " <> displayExpr b <> "`")
    ( Engine.Config
        { initialGas = FiniteGas 50,
          strategy = DepthFirstStrategy defaultDepthFirstStrategyOpts,
          rules = rulesSubtyping,
          exprAliases = [],
          goals = [mkGoal 0 $ a :<: b],
          shouldSuspend = \case
            Goal {atom = VarExpr _ :<: VarExpr _} -> True
            _ -> False,
          useIndexing = True

        }
    )

rulesSubtyping :: [Rule A C V]
rulesSubtyping =
  [ (mkRule "bool <: bool")
      []
      (bool :<: bool),
    (mkRule "int <: int")
      []
      (int :<: int),
    (mkRule "nat <: nat")
      []
      (nat :<: nat),
    (mkRule "nat <: int")
      []
      (nat :<: int),
    (mkRule "a' <: a , b <: b'  ⊢  a → b <: a' → b'")
      [ GoalHyp . mkHypGoal $ a' :<: a,
        GoalHyp . mkHypGoal $ b :<: b'
      ]
      ((a `arr` b) :<: (a' `arr` b'))
  ]
  where
    (a, a', b, b') = ("a", "a'", "b", "b'")

-- atoms

pattern (:<:) :: Expr C V -> Expr C V -> Atom A C V
pattern (:<:) s t = Atom "Subtype" [s, t]

-- expressions

int :: Expr C V
int = ConExpr (Con "int" [])

nat :: Expr C V
nat = ConExpr (Con "nat" [])

bool :: Expr C V
bool = ConExpr (Con "bool" [])

arr :: Expr C V -> Expr C V -> Expr C V
arr a b = ConExpr (Con "arr" [a, b])

displayExpr :: Expr C V -> String
displayExpr (ConExpr (Con "int" [])) = "int"
displayExpr (ConExpr (Con "nat" [])) = "nat"
displayExpr (ConExpr (Con "bool" [])) = "bool"
displayExpr (ConExpr (Con "arr" [a, b])) = displayExpr a <> " → " <> displayExpr b
displayExpr (VarExpr x@(Var {})) = x.labelVar
displayExpr e = prettyShow e
