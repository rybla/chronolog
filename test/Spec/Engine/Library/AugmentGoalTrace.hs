{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Engine.Library.AugmentGoalTrace (tests) where

import Chronolog.Engine as Engine
import Chronolog.Grammar
import Chronolog.Library.AugmentGoalTrace as AugmentGoalTrace
import Data.String (IsString (fromString))
import Spec.Engine.Common hiding (goldenDirpath)
import qualified Spec.Engine.Common as Common
import System.FilePath ((<.>), (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Text.PrettyPrint (hang, render, vcat)
import Text.PrettyPrint.HughesPJClass (Pretty (..))

goldenDirpath :: FilePath
goldenDirpath = Common.goldenDirpath </> "AugmentGoalTrace"

tests :: TestTree
tests =
  testGroup
    "AugmentGoalTrace"
    let mkTestGroup groupName cfg_engine_pre cfg_augmentGoalTrace =
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
              mkTest_Engine (groupName <> "_post") cfg_engine_post EngineSuccess,
              mkTest_Engine_visualization (groupName <> "_post") (groupName <> "_post" <> ".html") cfg_engine_post
            ]
          where
            cfg_engine_post = augmentGoalTrace cfg_augmentGoalTrace cfg_engine_pre
     in [ mkTestGroup
            "ex1"
            Engine.Config
              { initialGas = FiniteGas 50,
                rules =
                  [ mkRule "R1" [GoalHyp . mkHypGoal $ B] A,
                    mkRule "R2" [] B
                  ],
                goals =
                  [mkGoal 0 A],
                exprAliases = [],
                shouldSuspend = const False,
                strategy = DepthFirstStrategy defaultDepthFirstStrategyOpts,
                useIndexing = True
              }
            AugmentGoalTrace.Config {},
          mkTestGroup
            "ex2"
            Engine.Config
              { initialGas = FiniteGas 50,
                rules =
                  [ mkRule "R1" [GoalHyp . mkHypGoal $ B] A,
                    mkRule "R2" [] B
                  ],
                goals =
                  [mkHypGoal A, mkHypGoal A],
                exprAliases = [],
                shouldSuspend = const False,
                strategy = DepthFirstStrategy defaultDepthFirstStrategyOpts,
                useIndexing = True
              }
            AugmentGoalTrace.Config {}
        ]

pattern A :: Atom A C V
pattern A = Atom "A" []

pattern B :: Atom A C V
pattern B = Atom "B" []
