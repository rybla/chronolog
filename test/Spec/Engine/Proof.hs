{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module Spec.Engine.Proof (tests) where

import ControlledFixpoint.Engine as Engine
import ControlledFixpoint.Grammar
import Data.Function ((&))
import qualified Data.Map as Map
import Spec.Engine.Common
import Test.Tasty (TestName, TestTree, testGroup)
import Text.PrettyPrint (Doc, parens, text)
import Text.PrettyPrint.HughesPJClass (pPrint, render, (<+>))

tests :: TestTree
tests =
  testGroup
    "Proof"
    [tests_Norm]

tests_Norm :: TestTree
tests_Norm =
  testGroup
    "Norm"
    [ tests_Norm_v1
    ]

-- TODO: fix these tests, since they relied on unintended behavior of halting on first failed goal
tests_Norm_v1 :: TestTree
tests_Norm_v1 =
  testGroup
    "v1"
    [ mkTest_success 0,
      mkTest_success 1,
      -- mkTest_success (0 + 0),
      -- mkTest_success (1 + 0),
      -- mkTest_success (1 + 1),
      -- mkTest_success (0 + 2),
      -- mkTest_success (2 + 0),
      -- mkTest_success (3 + 4),
      mkTest_successWithProof 0 pZ,
      mkTest_successWithProof 1 (pS pZ),
      -- mkTest_successWithProof (0 + 0) (pPZ pZ pZ),
      -- mkTest_successWithProof (1 + 0) (pPZ (pS pZ) pZ),
      -- mkTest_successWithProof (1 + 1) (pPS (pS pZ) (pS pZ) (pPZ (pS pZ) pZ)),
      -- mkTest_successWithProof (0 + 2) (pPS pZ (pS (pS pZ)) (pPS pZ (pS pZ) (pPZ pZ pZ))),
      -- mkTest_successWithProof (2 + 0) (pPZ (pS (pS pZ)) pZ),
      -- mkTest_successWithProof (3 + 4) (pPS (pS (pS (pS pZ))) (pS (pS (pS (pS pZ)))) (pPS (pS (pS (pS pZ))) (pS (pS (pS pZ))) (pPS (pS (pS (pS pZ))) (pS (pS pZ)) (pPS (pS (pS (pS pZ))) (pS pZ) (pPZ (pS (pS (pS pZ))) pZ))))),
      mkTest_success 0
    ]
  where
    pZ = "Z⇓" :% []
    pS a = "S⇓" :% [a]
    _pPZ a b = "+Z⇓" :% [a, b]
    _pPS a b c = "+S⇓" :% [a, b, c]

    mkTest_success :: Expr C V -> TestTree
    mkTest_success x =
      mkTest (render (prettyExpr x <+> "⇓" <+> text (show y))) x $
        EngineSuccessWithSubst
          (Subst $ Map.fromList [("?out", fromIntegral y)])
      where
        y = interpret x

    mkTest_successWithProof :: Expr C V -> Expr C V -> TestTree
    mkTest_successWithProof x pf =
      mkTest (render (prettyExpr x <+> "⇓" <+> text (show y) <+> "via proof")) x $
        EngineSuccessWithSubst
          (Subst $ Map.fromList [("?out", fromIntegral y), ("?{in ⇓ out}", pf)])
      where
        y = interpret x

    mkTest :: TestName -> Expr C V -> EngineResult C V -> TestTree
    mkTest testName in_ =
      mkTest_Engine
        testName
        Engine.Config
          { goals = [mkGoal 0 $ Valid (in_ :⇓ "?out") "?{in ⇓ out}"],
            strategy = DepthFirstStrategy defaultDepthFirstStrategyOpts,
            rules = rules_v1,
            exprAliases = [],
            shouldSuspend = \case
              Goal {atom = Valid (VarExpr _ :⇓ VarExpr _) _} -> True
              _ -> False,
            initialGas = FiniteGas 50
          }

    rules_v1 :: [Rule A C V]
    rules_v1 =
      [ -- normal forms
        let ruleName = "Z⇓"
         in (mkRule (RuleName ruleName))
              []
              (Valid (Z :⇓ Z) $ ruleName :% []),
        let ruleName = "S⇓"
            (a, a', pf_a_norm_a') = ("?a", "?a'", "?{a ⇓ a'}")
         in (mkRule (RuleName ruleName))
              [GoalHyp . mkHypGoal $ Valid (a :⇓ a') pf_a_norm_a']
              (Valid (S a :⇓ S a') $ ruleName :% [pf_a_norm_a']),
        let ruleName = "+Z⇓"
            (a, a', pf_a_norm_a', b, pf_b_norm_Z) = ("?a", "?a'", "?{a ⇓ a'}", "b", "?{b ⇓ Z}")
         in (mkRule (RuleName ruleName))
              [ GoalHyp . mkHypGoal $ Valid (a :⇓ a') pf_a_norm_a',
                GoalHyp . mkHypGoal $ Valid (b :⇓ Z) pf_b_norm_Z
              ]
              (Valid (a + b :⇓ a') $ ruleName :% [pf_a_norm_a', pf_b_norm_Z]),
        let ruleName = "+S⇓"
            (a, a', pf_a_norm_a', b, b', pf_b_norm_Sb', c, pf_a'_plus_b'_norm_c) = ("?a", "?a'", "?{a ⇓ a'}", "?b", "?b'", "?{b ⇓ S b'}", "?c", "?{a' + b' ⇓ c}")
         in (mkRule (RuleName ruleName))
              [ GoalHyp . mkHypGoal $ Valid (a :⇓ a') pf_a_norm_a',
                GoalHyp . mkHypGoal $ Valid (b :⇓ S b') pf_b_norm_Sb',
                GoalHyp . mkHypGoal $ Valid (a' + b' :⇓ c) pf_a'_plus_b'_norm_c
              ]
              (Valid (a + b :⇓ S c) $ ruleName :% [pf_a_norm_a', pf_b_norm_Sb', pf_a'_plus_b'_norm_c])
      ]

interpret :: Expr C V -> Int
interpret Z = 0
interpret (S n) = 1 + interpret n
interpret (a :+ b) = interpret a + interpret b
interpret e = error . render $ "interpret" <+> pPrint e

prettyExpr :: Expr C V -> Doc
prettyExpr e | Just i <- toInt e = text (show i)
prettyExpr (S a) = parens ("S" <+> prettyExpr a)
prettyExpr (a :+ b) = prettyExpr a <+> "+" <+> prettyExpr b
prettyExpr e = error . render $ "prettyExpr" <+> pPrint e

toInt :: Expr C V -> Maybe Int
toInt Z = Just 0
toInt (S a) = do
  i <- toInt a
  Just (1 + i)
toInt _ = Nothing

-- nat

pattern Z :: Expr C V
pattern Z = ConExpr (Con "Z" [])

pattern S :: Expr C V -> Expr C V
pattern S n = ConExpr (Con "S" [n])

instance Num (Expr C V) where
  fromInteger n = replicate (fromInteger n) S & foldr ($) Z
  (+) = (:+)

-- definitions

pattern Valid :: Expr C V -> Expr C V -> Atom A C V
pattern Valid st pf = Atom "Valid" [st, pf]

pattern (:⇓) :: Expr C V -> Expr C V -> Expr C V
pattern (:⇓) a b = ConExpr (Con "Norm" [a, b])

infix 4 :⇓

pattern (:+) :: Expr C V -> Expr C V -> Expr C V
pattern a :+ b = ConExpr (Con "Add" [a, b])

infixl 6 :+
