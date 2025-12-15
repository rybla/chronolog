{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Engine.Library.AugmentDerivation (tests) where

import Chronolog.Engine as Engine
import Chronolog.Grammar
import Chronolog.Library.AugmentDerivation as AugmentDerivation
import Data.Coerce (coerce)
import Data.Functor ((<&>))
import Data.String (IsString (fromString))
import Spec.Engine.Common hiding (goldenDirpath)
import qualified Spec.Engine.Library.Common as Common
import System.FilePath ((<.>), (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Text.PrettyPrint (hang, render, vcat)
import Text.PrettyPrint.HughesPJClass (Pretty (..), prettyShow)

goldenDirpath :: FilePath
goldenDirpath = Common.goldenDirpath </> "AugmentDerivation"

tests :: TestTree
tests =
  testGroup
    "AugmentDerivation"
    [ testGroup
        "rules"
        [ goldenVsString
            "0_hypotheses"
            (goldenDirpath </> "rules" </> "0_hypotheses" <.> "golden")
            do
              return . fromString . prettyShow $
                [ (mkRule "R1")
                    []
                    Q ::
                    Rule A C V
                ]
                  <&> augmentDerivation_Rule
                    AugmentDerivation.Config {isDerivation = \a -> Just ("?" <> a)},
          goldenVsString
            "1_hypothesis"
            (goldenDirpath </> "rules" </> "1_hypothesis" <.> "golden")
            do
              return . fromString . prettyShow $
                [ (mkRule "R1")
                    [GoalHyp . mkHypGoal $ P1]
                    Q
                ]
                  <&> augmentDerivation_Rule
                    AugmentDerivation.Config {isDerivation = \a -> Just ("?" <> coerce a)},
          goldenVsString
            "3_hypotheses"
            (goldenDirpath </> "rules" </> "3_hypotheses" <.> "golden")
            do
              return . fromString . prettyShow $
                [ (mkRule "R1")
                    [ GoalHyp . mkHypGoal $ P1,
                      GoalHyp . mkHypGoal $ P2,
                      GoalHyp . mkHypGoal $ P3
                    ]
                    Q
                ]
                  <&> augmentDerivation_Rule
                    AugmentDerivation.Config {isDerivation = \a -> Just ("?" <> coerce a)},
          goldenVsString
            "3_hypotheses_2_derivations"
            (goldenDirpath </> "rules" </> "3_hypotheses_2_derivations" <.> "golden")
            do
              return . fromString . prettyShow $
                [ (mkRule "R1")
                    [ GoalHyp . mkHypGoal $ B,
                      GoalHyp . mkHypGoal $ P1,
                      GoalHyp . mkHypGoal $ P2
                    ]
                    Q
                ]
                  <&> augmentDerivation_Rule
                    AugmentDerivation.Config
                      { isDerivation = \case
                          a | a `elem` ["P1", "P2"] -> Just ("?" <> coerce a)
                          _ | otherwise -> Nothing
                      }
        ],
      let mkTestGroup groupName cfg_engine_pre cfg_augmentDerivation =
            testGroup
              groupName
              [ goldenVsString (groupName <> "_pre_rules") (goldenDirpath </> "solutions" </> (groupName <> "_pre_rules") <.> "golden") do
                  return . fromString . render $
                    vcat
                      [ hang "rules =" 2 (pPrint cfg_engine_pre.rules),
                        hang "goals =" 2 (pPrint cfg_engine_pre.goals)
                      ],
                mkTest_Engine (groupName <> "_pre") cfg_engine_pre EngineSuccess,
                goldenVsString (groupName <> "_post_rules") (goldenDirpath </> "solutions" </> (groupName <> "_post_rules") <.> "golden") do
                  return . fromString . render $
                    vcat
                      [ hang "rules =" 2 (pPrint cfg_engine_post.rules),
                        hang "goals =" 2 (pPrint cfg_engine_post.goals)
                      ],
                mkTest_Engine (groupName <> "_post") cfg_engine_post EngineSuccess
              ]
            where
              cfg_engine_post = augmentDerivation cfg_augmentDerivation cfg_engine_pre
       in testGroup
            "solutions"
            [ mkTestGroup
                "ex1"
                Engine.Config
                  { initialGas = FiniteGas 50,
                    rules =
                      [ (mkRule "T_Z")
                          []
                          (T Z),
                        (mkRule "T_Sn")
                          [ GoalHyp . mkHypGoal $ T "n",
                            GoalHyp . mkHypGoal $ T "n"
                          ]
                          (T (S "n"))
                      ],
                    goals = [mkGoal 0 $ T (S (S (S (S Z))))],
                    exprAliases = [],
                    shouldSuspend = const False,
                    strategy = DepthFirstStrategy defaultDepthFirstStrategyOpts,
                    useIndexing = True
                  }
                AugmentDerivation.Config
                  { isDerivation = \a -> Just ("?" <> coerce a)
                  }
            ]
    ]

pattern P1 :: Atom A C V
pattern P1 = Atom "P1" []

pattern P2 :: Atom A C v
pattern P2 = Atom "P2" []

pattern P3 :: Atom A C v
pattern P3 = Atom "P3" []

pattern B :: Atom A C v
pattern B = Atom "B" []

pattern Q :: Atom A C v
pattern Q = Atom "Q" []

pattern Z :: Expr C V
pattern Z = ConExpr (Con "Z" [])

pattern S :: Expr C V -> Expr C V
pattern S n = ConExpr (Con "S" [n])

pattern T :: Expr C V -> Atom A C V
pattern T n = Atom "T" [n]
