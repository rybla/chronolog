{-# LANGUAGE OverloadedStrings #-}

module Spec.Unification (tests) where

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (StateT (runStateT))
import Control.Monad.Writer (WriterT (runWriterT))
import Chronolog.Grammar
import Chronolog.Unification (unifyAtom)
import qualified Chronolog.Unification as Unification
import Data.Function ((&))
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Text.PrettyPrint (render, (<+>))
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))

type A = String

type C = String

type V = String

mkAtom :: A -> [Expr C V] -> Atom A C V
mkAtom = Atom

tests :: TestTree
tests =
  testGroup
    "Unification"
    [ mkTest
        "ex1"
        (mkAtom "C2" ["X", "X"])
        (mkAtom "C2" ["C0" :% [], "C0" :% []])
        (mkAtom "C2" ["C0" :% [], "C0" :% []])
    ]

mkTest :: TestName -> Atom A C V -> Atom A C V -> Atom A C V -> TestTree
mkTest testName atom1 atom2 atomExpected = testCase testName do
  result <-
    atom1 `unifyAtom` atom2
      & (`runReaderT` Unification.Ctx {exprAliases = []})
      & runExceptT
      & (`runStateT` (Unification.Env {_sigma = emptySubst}))
      & runExceptT
      & runWriterT
  case result of
    (Left msg, _) -> assertFailure . render $ "catastrophic error:" <+> pPrint msg
    (Right (Left err, _), _) -> assertFailure . render $ "error:" <+> pPrint err
    (Right (Right atomActual, _), _) -> atomActual @?= atomExpected
