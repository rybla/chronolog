{-# LANGUAGE OverloadedStrings #-}

module Spec.Engine.PathIndexing (tests) where

import Chronolog.Grammar
import Chronolog.Indexing
import Spec.Engine.Common
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

tests :: TestTree
tests =
    testGroup
        "PathIndexing"
        [ mkIndexingTest
            "path_indexing_1"
            [ mkRule
                (RuleName "P")
                []
                (Atom "P" [])
            ]
            -- Goal: P
            (mkGoal 0 (Atom "P" []))
            ["P"]
        , mkIndexingTest
            "path_indexing_2"
            [ mkRule
                (RuleName "P 1")
                []
                (Atom "P" ["1" :% []])
            , mkRule
                (RuleName "P 2")
                []
                (Atom "P" ["2" :% []])
            ]
            -- Goal: P 1
            (mkGoal 0 (Atom "P" [ConExpr (Con "1" [])]))
            ["P 1"]
        , mkIndexingTest
            "path_indexing_3"
            [ mkRule
                (RuleName "P (1 2)")
                []
                (Atom "P" ["1" :% ["2" :% []]])
            , mkRule
                (RuleName "P (1 x)")
                []
                (Atom "P" ["1" :% [VarExpr "x"]])
            ]
            -- Goal: P (1 2)
            (mkGoal 0 (Atom "P" ["1" :% ["2" :% []]]))
            ["P (1 2)", "P (1 x)"]
        , mkIndexingTest
            "path_indexing_4"
            [ mkRule
                (RuleName "P (a 1 x) (b y)")
                []
                ( Atom
                    "P"
                    [ "a" :% ["1" :% [], VarExpr "x"]
                    , "b" :% [VarExpr "y"]
                    ]
                )
            , mkRule
                (RuleName "P (a x (b y)) z")
                []
                ( Atom
                    "P"
                    [ "a" :% [VarExpr "x", "b" :% [VarExpr "y"]]
                    , "b" :% [VarExpr "z"]
                    ]
                )
            , mkRule
                (RuleName "P (b x) (b y)")
                []
                ( Atom
                    "P"
                    [ "b" :% [VarExpr "x"]
                    , "b" :% [VarExpr "y"]
                    ]
                )
            , mkRule
                (RuleName "P x (a x y)")
                []
                (Atom "P" [VarExpr "x", "a" :% [VarExpr "x", VarExpr "y"]])
            ]
            -- Goal: P (a 1 (b 1)) (b 1)
            ( mkGoal
                0
                ( Atom
                    "P"
                    [ "a" :% ["1" :% [], "b" :% ["1" :% []]]
                    , "b" :% ["1" :% []]
                    ]
                )
            )
            ["P (a 1 x) (b y)", "P (a x (b y)) z"]
        ]

mkIndexingTest :: TestName -> [Rule A C V] -> Goal A C V -> [String] -> TestTree
mkIndexingTest testName rules goal expected = testCase testName do
    let trie = buildIndex [] rules
        filterRules = filterPathIndexing [] goal trie
        filtered = map (\(Rule (RuleName rname) _ _ _) -> rname) filterRules
     in if filtered == expected
            then assertBool "" True
            else
                assertFailure $
                    "Got "
                        ++ show filtered
                        ++ " instead of "
                        ++ show expected
                        ++ "\ntrie: \n"
                        ++ genGraphviz trie
