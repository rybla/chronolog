{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Engine.Add (tests) where

import ControlledFixpoint.Engine as Engine
import ControlledFixpoint.Grammar
import Spec.Engine.Common
import Test.Tasty (TestTree, testGroup)
import Text.PrettyPrint (render, (<+>))
import Text.PrettyPrint.HughesPJClass (pPrint)

tests :: TestTree
tests =
  testGroup
    "Add"
    [ mkTest 0 0 0 EngineSuccess,
      mkTest 0 1 1 EngineSuccess,
      mkTest 1 0 1 EngineSuccess,
      mkTest 1 2 3 EngineSuccess,
      mkTest 1 2 2 EngineFailure
    ]

mkTest :: Int -> Int -> Int -> EngineResult C V -> TestTree
mkTest a b c =
  mkTest_Engine
    (render $ pPrint a <+> "+" <+> pPrint b <+> "=" <+> pPrint c)
    ( Config
        { initialGas = FiniteGas 50,
          strategy = DepthFirstStrategy defaultDepthFirstStrategyOpts,
          rules = rulesAdd,
          exprAliases = [],
          goals = [mkGoal 0 $ fromIntegral a :+ fromIntegral b :== fromIntegral c],
          shouldSuspend = const False
        }
    )

rulesAdd :: [Rule A C V]
rulesAdd =
  [ (mkRule "0+")
      []
      (0 + x :== x),
    (mkRule "S+")
      [GoalHyp . mkHypGoal $ x :+ y :== z]
      (S x :+ y :== S z)
  ]
  where
    (x, z, y) = ("x", "y", "z")

pattern (:==) :: Expr C V -> Expr C V -> Atom A C V
pattern x :== y = Atom "Equal" [x, y]

infix 4 :==

pattern (:+) :: Expr C V -> Expr C V -> Expr C V
pattern x :+ y = ConExpr (Con "Add" [x, y])

infixl 6 :+

pattern S :: Expr C V -> Expr C V
pattern S x = ConExpr (Con "S" [x])

pattern Z :: Expr C V
pattern Z = ConExpr (Con "Z" [])

instance Num (Expr C V) where
  (+) = (:+)
  fromInteger n | n < 0 = undefined
  fromInteger 0 = Z
  fromInteger n = S $ fromInteger (n - 1)
