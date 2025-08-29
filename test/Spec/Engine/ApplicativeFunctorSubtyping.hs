{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use ++" #-}

module Spec.Engine.ApplicativeFunctorSubtyping (tests) where

import Chronolog.Engine as Engine
import Chronolog.Grammar
import qualified Data.Map as Map
import Spec.Engine.Common
import Test.Tasty (TestTree, testGroup)
import Text.PrettyPrint (Doc, parens, render, (<+>))
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))
import Prelude hiding (fmap, pure)

tests :: TestTree
tests =
  testGroup
    "ApplicativeFunctorSubtyping"
    [tests_v1]

tests_v1 :: TestTree
tests_v1 =
  testGroup
    "v1"
    [ mkTest
        (Int :-> Bool)
        (List :@ Int :-> List :@ Bool)
        (fmap functorList (subArrow subInt subBool)),
      mkTest
        (Int :-> Bool)
        (List :@ Nat :-> List :@ Bool)
        (fmap functorList (subArrow subNatOfInt subBool)),
      mkTest
        Bool
        (List :@ Bool)
        (pure functorList subBool),
      mkTest
        Bool
        (List :@ (List :@ (List :@ (List :@ (List :@ Bool)))))
        (pure functorList (pure functorList (pure functorList (pure functorList (pure functorList subBool)))))
    ]
  where
    mkTest s t pf' =
      let pf = "?{s <: t}"
       in mkTest_Engine
            (render $ prettyExpr (s :<: t))
            ( Engine.Config
                { goals = [mkGoal 0 $ Valid (s :<: t) (VarExpr pf)],
                  rules = rules_v1,
                  initialGas = FiniteGas 50,
                  strategy = DepthFirstStrategy defaultDepthFirstStrategyOpts,
                  exprAliases = [],
                  shouldSuspend = \case
                    Goal {atom = Valid (Functor (VarExpr _)) _} -> True
                    Goal {atom = Valid (VarExpr _ :<: VarExpr _) _} -> True
                    _ -> False
                }
            )
            (EngineSuccessWithSubst $ Subst $ Map.fromList [(pf, pf')])

    rules_v1 :: [Rule A C V]
    rules_v1 =
      concat
        [ -- core rules
          [ mkRule
              "subArrow"
              [ GoalHyp . mkHypGoal $ Valid (a' :<: a) a'_subtype_a,
                GoalHyp . mkHypGoal $ Valid (b :<: b') b_subtype_b'
              ]
              (Valid ((a :-> b) :<: (a' :-> b')) (subArrow a'_subtype_a b_subtype_b')),
            mkRule
              "subFunctor"
              [ GoalHyp . mkHypGoal $ Valid (Functor f) functor_f,
                GoalHyp . mkHypGoal $ Valid (a :<: a') a_subtype_a'
              ]
              (Valid (f :@ a :<: f :@ a') (subFunctor functor_f a_subtype_a')),
            mkRule
              "fmap"
              [ GoalHyp . mkHypGoal $ Valid (Functor f) functor_f,
                GoalHyp . mkHypGoal $ Valid (a :-> b :<: a' :-> b') a_arrow_a'_subtype_b_arrow_b'
              ]
              (Valid (a :-> b :<: f :@ a' :-> f :@ b') (fmap functor_f a_arrow_a'_subtype_b_arrow_b')),
            mkRule
              "pure"
              [ GoalHyp . mkHypGoal $ Valid (Functor f) functor_f,
                GoalHyp . mkHypGoal $ Valid (a :<: a') a_subtype_a'
              ]
              (Valid (a :<: f :@ a') (pure functor_f a_subtype_a'))
          ],
          -- datatype rules
          [ mkRule
              "subNatOfInt"
              []
              (Valid (Nat :<: Int) subNatOfInt),
            mkRule
              "functorList"
              []
              (Valid (Functor List) functorList),
            mkRule
              "subBool"
              []
              (Valid (Bool :<: Bool) subBool),
            mkRule
              "subInt"
              []
              (Valid (Int :<: Int) subInt),
            mkRule
              "subNat"
              []
              (Valid (Nat :<: Nat) subNat)
          ]
        ]
      where
        f = "?f"
        functor_f = "?{Functor f}"

        a = "?a"
        a' = "?a'"
        a'_subtype_a = "?{a' <: a}"
        a_subtype_a' = "?{a <: a'}"

        b = "?b"
        b' = "?b'"
        b_subtype_b' = "?{b <: b'}"

        a_arrow_a'_subtype_b_arrow_b' = "?{a -> a' <: b -> b'}"

subArrow :: Expr C V -> Expr C V -> Expr C V
subArrow a'_subtype_a b_subtype_b' = "subArrow" :% [a'_subtype_a, b_subtype_b']

subFunctor :: Expr C V -> Expr C V -> Expr C V
subFunctor functor_f a_subtype_a' = "subFunctor" :% [functor_f, a_subtype_a']

fmap :: Expr C V -> Expr C V -> Expr C V
fmap functor_f a_arrow_a'_subtype_b_arrow_b' = "fmap" :% [functor_f, a_arrow_a'_subtype_b_arrow_b']

pure :: Expr C V -> Expr C V -> Expr C V
pure functor_f a_subtype_a' = "pure" :% [functor_f, a_subtype_a']

subNatOfInt :: Expr C V
subNatOfInt = "subNatOfInt" :% []

functorList :: Expr C V
functorList = "functorList" :% []

subBool :: Expr C V
subBool = "subBool" :% []

subInt :: Expr C V
subInt = "subInt" :% []

subNat :: Expr C V
subNat = "subNat" :% []

prettyExpr :: Expr C V -> Doc
prettyExpr (s :<: t) = prettyExpr s <+> "<:" <+> prettyExpr t
prettyExpr (Functor f) = "Functor" <+> prettyExpr f
prettyExpr (s :-> t) = parens (prettyExpr s <+> "->" <+> prettyExpr t)
prettyExpr (f :@ t) = parens (prettyExpr f <+> prettyExpr t)
prettyExpr List = "List"
prettyExpr Bool = "Bool"
prettyExpr Nat = "Nat"
prettyExpr Int = "Int"
prettyExpr e = pPrint e

-- atoms

pattern Valid :: Expr C V -> Expr C V -> Atom A C V
pattern Valid st pf = Atom "Valid" [st, pf]

-- relations

pattern (:<:) :: Expr C V -> Expr C V -> Expr C V
pattern (:<:) s t = ConExpr (Con "Subtype" [s, t])

infix 4 :<:

pattern Functor :: Expr C V -> Expr C V
pattern Functor f = ConExpr (Con "Functor" [f])

-- expressions

pattern (:->) :: Expr C V -> Expr C V -> Expr C V
pattern a :-> b = ConExpr (Con "Arrow" [a, b])

infixr 5 :->

pattern (:@) :: Expr C V -> Expr C V -> Expr C V
pattern f :@ a = ConExpr (Con "App" [f, a])

infixl 6 :@

pattern List :: Expr C V
pattern List = ConExpr (Con "List" [])

pattern Bool :: Expr C V
pattern Bool = ConExpr (Con "Bool" [])

pattern Nat :: Expr C V
pattern Nat = ConExpr (Con "Nat" [])

pattern Int :: Expr C V
pattern Int = ConExpr (Con "Int" [])
