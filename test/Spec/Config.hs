{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Spec.Config where

import qualified Chronolog.Common.Msg as Msg

data Verbosity
  = SilentVerbosity
  | NormalVerbosity
  | LoggingVerbosity Msg.Level
  deriving (Show, Eq, Ord)

verbosity :: Verbosity
-- verbosity = NormalVerbosity
verbosity = LoggingVerbosity (Msg.Level 1)
