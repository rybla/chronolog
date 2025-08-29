-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TypeApplications #-}
-- {-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

-- import Control.Monad (void)
-- import Control.Monad.Except (runExceptT)
-- import Control.Monad.Writer (WriterT (runWriterT))
-- import qualified Chronolog.Engine as Engine
-- import Chronolog.Grammar
-- import Data.String (IsString (fromString))
-- import Text.PrettyPrint (hang, render)
-- import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))
-- import Utility

-- rulesSubtyping :: [Rule]
-- rulesSubtyping =
--   [ Rule
--       { name = "bool <: bool",
--         hyps = [],
--         conc = bool `subtype` bool
--       },
--     Rule
--       { name = "int <: int",
--         hyps = [],
--         conc = int `subtype` int
--       },
--     Rule
--       { name = "nat <: nat",
--         hyps = [],
--         conc = nat `subtype` nat
--       },
--     Rule
--       { name = "nat <: int",
--         hyps = [],
--         conc = nat `subtype` int
--       },
--     Rule
--       { name = "a' <: a , b <: b'  |-  a -> b <: a' -> b'",
--         hyps =
--           [ GoalHyp $ a' `subtype` a,
--             GoalHyp $ b `subtype` b'
--           ],
--         conc = (a `arr` b) `subtype` (a' `arr` b')
--       }
--       {- Rule
--         { name = "map",
--           hyps =
--             [ GoalHyp $ (a `arr` b) `subtype` (a' `arr` b'),
--               GoalHyp $ functor f
--             ],
--           conc = (a `arr` b) `subtype` ((f `app` a) `arr` (f `app` b))
--         } -}
--   ]
--   where
--     (f, a, a', b, b') = ("f", "a", "a'", "b", "b'")

-- -- atoms

-- subtype :: Expr -> Expr -> Atom
-- subtype a b = Atom "subtype" $ ConExpr (Con "subtype" [a, b])

-- functor :: Expr -> Atom
-- functor f = Atom "functor" $ ConExpr (Con "functor" [f])

-- -- expressions

-- var :: String -> Expr
-- var x = VarExpr (Var x Nothing)

-- instance IsString Expr where fromString = var

-- int :: Expr
-- int = ConExpr (Con "int" [])

-- nat :: Expr
-- nat = ConExpr (Con "nat" [])

-- bool :: Expr
-- bool = ConExpr (Con "bool" [])

-- arr :: Expr -> Expr -> Expr
-- arr a b = ConExpr (Con "arr" [a, b])

-- app :: Expr -> Expr -> Expr
-- app f a = ConExpr (Con "app" [f, a])

-- main :: IO ()
-- main = do
--   let cfg =
--         Engine.Config
--           { initialGas = 100,
--             rules = rulesSubtyping,
--             -- goals = [subtype (arr "x" "y") (arr "x'" "y'")],
--             goals = ["x" `subtype` ("y" `arr` "z")],
--             shouldSuspend = \case
--               Atom _ (ConExpr (Con "subtype" [VarExpr _, VarExpr _])) -> True
--               _ -> False
--           }
--   (err_or_envs, msgs) <-
--     Engine.run cfg
--       & runExceptT
--       & runWriterT
--   putStrLn "================================"
--   putStrLn " logs "
--   putStrLn "================================"
--   void $ msgs <&>>= \msg -> putStrLn $ render $ pPrint msg
--   putStrLn "================================"
--   putStrLn " result "
--   putStrLn "================================"
--   case err_or_envs of
--     Left err -> putStrLn $ render (hang "err:" 2 $ pPrint err)
--     Right envs -> putStrLn $ render (hang "envs:" 2 $ bullets (envs <&> pPrint))
--   return ()

main :: IO ()
main = return ()
