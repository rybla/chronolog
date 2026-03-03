{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Engine.AddRelation (tests) where

import Chronolog.Engine as Engine
import Chronolog.Grammar
import Spec.Engine.Common
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "AddRelation"
    [ mkTest "test1" (S Z) Z (S Z),
      mkTest "test2" (S (S Z)) (S Z) (S (S (S Z))),
      mkTest "test3" (S "x") Z (S (S (S Z)))
    ]

type N = Expr C V

mkTest :: String -> N -> N -> N -> TestTree
mkTest nm a b c =
  mkTest_Engine_visualization
    nm
    (nm ++ ".html")
    ( Config
        { initialGas = FiniteGas 50,
          strategy = DepthFirstStrategy defaultDepthFirstStrategyOpts,
          rules = rulesAdd,
          exprAliases = [],
          goals = [mkGoal 0 $ Add a b c],
          shouldSuspend = const False,
          useIndexing = True
        }
    )
    

rulesAdd :: [Rule A C V]
rulesAdd =
  [ (mkRule "0+")
      []
      (Add Z x x),
    (mkRule "S+")
      [GoalHyp . mkHypGoal $ Add x y z]
      (Add (S x) y (S z))
  ]
  where
    (x, z, y) = ("x", "y", "z")

pattern Add :: Expr C V -> Expr C V -> Expr C V -> Atom A C V
pattern Add x y z = Atom "Add" [x, y, z]

pattern S :: Expr C V -> Expr C V
pattern S x = ConExpr (Con "S" [x])

pattern Z :: Expr C V
pattern Z = ConExpr (Con "Z" [])

