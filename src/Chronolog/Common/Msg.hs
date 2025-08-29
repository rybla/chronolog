{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Chronolog.Common.Msg where

import Text.PrettyPrint (Doc, brackets, nest, vcat, (<+>))
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))
import Utility

data Msg = Msg
  { title :: Doc,
    contents :: [Doc],
    level :: Level
  }
  deriving (Show, Eq)

instance Pretty Msg where
  pPrint m =
    brackets (pPrint m.level)
      <+> vcat (m.title : ([nest 2 (bullets m.contents) | not (null m.contents)]))

mk :: Int -> Doc -> Msg
mk l title =
  Msg
    { level = Level l,
      title,
      contents = mempty
    }

addContent :: Doc -> Msg -> Msg
addContent content msg = msg {contents = msg.contents <> [content]}

newtype Level = Level Int
  deriving (Show, Eq, Ord)

instance Pretty Level where pPrint (Level l) = "L" <> pPrint l
