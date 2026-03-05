{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Engine.ContinueAfterFailure (tests) where

import Chronolog.Engine as Engine
import Chronolog.Grammar
import Spec.Engine.Common
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "ContinueAfterFailure"
    let nm = "caf" in
     [mkTest_Engine_visualization
      nm
      (nm ++ ".html")
      (Config
        { initialGas = FiniteGas 50,
          strategy = DepthFirstStrategy defaultDepthFirstStrategyOpts,
          rules = rulesCAF,
          exprAliases = [],
          goals = [mkGoal 0 $ G J "z"],
          shouldSuspend = const False,
          useIndexing = True
        }
      )]

gh :: Atom A C V -> Hyp A C V
gh = GoalHyp . mkHypGoal

rulesCAF :: [Rule A C V]
rulesCAF =
  [ (mkRule "FI")
      []
      (F I),
    (mkRule "GFF")
      [gh $ F "x",
       gh $ F "y" ]
      (G "x" "y")
  ]

pattern F :: Expr C V -> Atom A C V
pattern F x = Atom "F" [x]

pattern G :: Expr C V -> Expr C V -> Atom A C V
pattern G x y = Atom "G" [x,y]

pattern I :: Expr C V
pattern I = ConExpr (Con "I" [])

pattern J :: Expr C V
pattern J = ConExpr (Con "J" [])

