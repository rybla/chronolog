{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use ||" #-}
{-# HLINT ignore "Redundant $" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use if" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Chronolog.Engine where

import qualified Chronolog.Common as Common
import Chronolog.Common.Msg (Msg)
import qualified Chronolog.Common.Msg as Msg
import qualified Chronolog.Freshening as Freshening
import Chronolog.Grammar
import Chronolog.Indexing (filterPathIndexing)
import qualified Chronolog.Indexing as Indexing
import qualified Chronolog.Unification as Unification
import Control.Lens ((^.))
import Control.Monad (foldM, unless, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT (..), evalStateT, execStateT, get, gets, modify, runState, runStateT)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Writer (WriterT)
import qualified Control.Monad.Writer as Writer
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.List.Safe as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import ListT (ListT, cons, toList)
import Text.PrettyPrint.HughesPJClass (Doc, Pretty (pPrint), hang, hcat, text, (<+>))
import Utility
import Prelude hiding (init)

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

-- TODO maybe use a ConfigBuilder here so that goals get assigned indices appropriately

-- | Engine configuration
--
-- The `shouldSuspend` predicate specifies when to suspend a goal `Atom`. For
-- example, you might want to suspend a goal that would trivially unify with
-- many rule heads, such as something like `IsTrue a` (where `a` might be
-- constrained in other goals).
data Config a c v = Config
  { initialGas :: Gas,
    rules :: [Rule a c v],
    goals :: [Goal a c v],
    shouldSuspend :: Goal a c v -> Bool,
    exprAliases :: [ExprAlias c v],
    strategy :: Strategy,
    useIndexing :: Bool
  }

defaultConfig :: Config a c v
defaultConfig =
  Config
    { initialGas = InfiniteGas,
      rules = [],
      goals = [],
      shouldSuspend = const False,
      exprAliases = [],
      strategy = DepthFirstStrategy defaultDepthFirstStrategyOpts,
      useIndexing = True
    }

instance (Pretty a, Pretty c, Pretty v) => Pretty (Config a c v) where
  pPrint cfg =
    hang "engine config" 4 . bullets $
      [ "initialGas =" <+> pPrint cfg.initialGas,
        "rules =" <+> pPrint cfg.rules,
        "goals =" <+> pPrint cfg.goals,
        "shouldSuspend =" <+> text "<function>",
        "exprAliases =" <+> text "<function>",
        "strategy =" <+> pPrint cfg.strategy,
        "useIndexing =" <+> pPrint cfg.useIndexing
      ]

type T a c v m =
  (ReaderT (Ctx a c v))
    ( (StateT (Env a c v))
        ( ListT
            ( (StateT Gas) -- gas is shared among branches, rather than each branch getting it's own copy like Env
                ( (ExceptT (Error, Env a c v))
                    ( (WriterT (Trace a c v))
                        ( Common.T m
                        )
                    )
                )
            )
        )
    )

liftT :: (Monad m) => Common.T m x -> T a c v m x
liftT = lift . lift . lift . lift . lift . lift

type T' a c v m =
  (ReaderT (Ctx a c v))
    ( (StateT (Env a c v))
        ( (ExceptT (Error, Env a c v))
            ( (WriterT (Trace a c v))
                ( Common.T m
                )
            )
        )
    )

runT' :: (Monad m) => T' a c v m x -> T a c v m (x, Env a c v)
runT' m = do
  ctx <- ask
  env <- get
  m
    & (`runReaderT` ctx)
    & (`runStateT` env)
    & lift . lift . lift . lift

data Error
  = OutOfGas
  deriving (Eq, Show)

data Trace a c v = Trace
  { traceGoals :: Map GoalIndex (Goal a c v),
    traceSteps :: Map GoalIndex [Step a c v]
  }

instance Semigroup (Trace a c v) where
  t1 <> t2 =
    Trace
      { -- t1 takes precedence
        traceGoals = Map.unionWith const t1.traceGoals t2.traceGoals,
        traceSteps = Map.unionWith (<>) t1.traceSteps t2.traceSteps
      }

instance Monoid (Trace a c v) where
  mempty =
    Trace
      { traceGoals = Map.empty,
        traceSteps = Map.empty
      }

-- | Engine context
data Ctx a c v = Ctx
  { config :: Config a c v
  }

instance (Pretty a, Pretty c, Pretty v) => Pretty (Ctx a c v) where
  pPrint ctx =
    hang "engine context:" 2 . bullets $
      [ hang "rules =" 2 . bullets $
          ctx.config.rules <&> pPrint
      ]

-- | Engine environment
data Env a c v = Env
  { freshCounter_vars :: Int,
    freshCounter_goals :: Int,
    rules :: [Rule a c v],
    pathIndex :: Indexing.Trie a c v,
    activeGoals :: [Goal a c v],
    suspendedGoals :: [Goal a c v],
    failedGoals :: [Goal a c v],
    sigma :: Subst c v,
    stepsRev :: [Step a c v]
  }

instance (Pretty a, Pretty c, Pretty v) => Pretty (Env a c v) where
  pPrint env =
    hang "engine environment:" 2 . bullets $
      [ "rules =" <+> "...",
        "activeGoals =" <+> pPrint env.activeGoals,
        "suspendedGoals =" <+> pPrint env.suspendedGoals,
        "failedGoals =" <+> pPrint env.failedGoals,
        hang "steps =" 2 . bullets $
          env.stepsRev & reverse <&> pPrint,
        "sigma =" <+> pPrint env.sigma
      ]

substEnv :: (Ord v) => Subst c v -> Env a c v -> Env a c v
substEnv sigma env =
  env
    { rules = substRule sigma <$> env.rules,
      activeGoals = substGoal sigma <$> env.activeGoals,
      suspendedGoals = substGoal sigma <$> env.suspendedGoals,
      failedGoals = substGoal sigma <$> env.failedGoals,
      sigma = composeSubst_unsafe sigma env.sigma
    }

-- | The proof-search strategy.
data Strategy
  = -- |
    -- A breadth-first strategy:
    --   - subgoals are inserted at the end of the list of subgoals to solve
    --     next, so they are to be solved after any pre-existing subgoals
    BreadthFirstStrategy
  | -- |
    -- A depth-first strategy:
    --   - subgoals are inserted at the beginning of the list of subgoals to
    --     solve next, so they are to be solved before any pre-existing subgoals
    DepthFirstStrategy DepthFirstStrategyOpts
  deriving (Eq, Show)

instance Pretty Strategy where
  pPrint = text . show

data DepthFirstStrategyOpts = DepthFirstStrategyOpts
  { -- | Whether or not to consider failing branches. `agressive == True`
    -- essentially implies that all goals are treated as required.
    aggressiveDepthFirstStrategyOpt :: Bool
  }
  deriving (Eq, Show)

defaultDepthFirstStrategyOpts :: DepthFirstStrategyOpts
defaultDepthFirstStrategyOpts =
  DepthFirstStrategyOpts
    { aggressiveDepthFirstStrategyOpt = False
    }

isAggressiveStrategy :: Strategy -> Bool
isAggressiveStrategy (DepthFirstStrategy opts) = opts.aggressiveDepthFirstStrategyOpt
isAggressiveStrategy _ = False

instance Pretty DepthFirstStrategyOpts where
  pPrint = text . show

instance Pretty Error where
  pPrint OutOfGas = "out of gas"

data Gas
  = FiniteGas Int
  | InfiniteGas
  deriving (Eq, Show)

instance Pretty Gas where
  pPrint (FiniteGas n) = pPrint n
  pPrint InfiniteGas = "∞"

isDepletedGas :: Gas -> Bool
isDepletedGas (FiniteGas n) = n <= 0
isDepletedGas InfiniteGas = False

decrementGas :: Gas -> Gas
decrementGas (FiniteGas n) = FiniteGas (n - 1)
decrementGas InfiniteGas = InfiniteGas

data Step a c v
  = ApplyRuleStep
      { goal :: Goal a c v,
        ruleName :: RuleName,
        sigma :: Subst c v,
        subgoals :: [Goal a c v]
      }
  | FailureStep
      { goal :: Goal a c v,
        reason :: Doc
      }
  | SuspendStep
      { goal :: Goal a c v,
        reason :: Doc
      }
  | ResumeStep
      { goal :: Goal a c v,
        reason :: Doc
      }
  | SolveStep
      { goal :: Goal a c v,
        sigma :: Subst c v,
        reason :: Doc
      }

instance (Pretty a, Pretty c, Pretty v) => Pretty (Step a c v) where
  pPrint (ApplyRuleStep {..}) = "apply" <+> hcat [pPrint ruleName, ":"] <+> pPrint goal <+> "<==" <+> pPrint subgoals <+> "with" <+> pPrint sigma
  pPrint (FailureStep {..}) = "fail" <+> pPrint goal <+> "because" <+> reason
  pPrint (SuspendStep {..}) = "suspend" <+> pPrint goal <+> "because" <+> reason
  pPrint (ResumeStep {..}) = "resume" <+> pPrint goal <+> "because" <+> reason
  pPrint (SolveStep {..}) = "solve" <+> pPrint goal <+> "via" <+> pPrint sigma <+> "because" <+> reason

--------------------------------------------------------------------------------
-- functions
--------------------------------------------------------------------------------

mkCtx :: Config a c v -> Ctx a c v
mkCtx cfg =
  Ctx
    { config = cfg
    }

mkEnv :: (Ord a, Ord c) => Config a c v -> Env a c v
mkEnv cfg =
  Env
    { rules = cfg.rules,
      pathIndex = if cfg.useIndexing then Indexing.buildIndex cfg.exprAliases cfg.rules else Indexing.emptyTrie,
      activeGoals = cfg.goals,
      suspendedGoals = mempty,
      failedGoals = mempty,
      freshCounter_vars = 0,
      freshCounter_goals = (cfg.goals & fmap (fromMaybe (-1) . goalIndex) & maximum) + 1,
      stepsRev = [],
      sigma = emptySubst
    }

-- | To run the solver, you must use either `runConfig` or `runEnv`.
runConfig ::
  (Monad m, Ord a, Ord c, Ord v, Pretty a, Pretty c, Pretty v, Show a, Show c, Show v) =>
  Config a c v ->
  WriterT (Trace a c v) (Common.T m) (Either (Error, Env a c v) [Env a c v])
runConfig cfg = do
  lift $ Writer.tell [Msg.mk 0 "Engine.run"]
  let env0 = mkEnv cfg
  runEnv cfg env0

-- | To run the solver, you must use either `runConfig` or `runEnv`.
runEnv ::
  (Monad m, Ord a, Ord c, Ord v, Pretty a, Pretty c, Pretty v, Show a, Show c, Show v) =>
  Config a c v ->
  Env a c v ->
  WriterT (Trace a c v) (Common.T m) (Either (Error, Env a c v) [Env a c v])
runEnv cfg env0 = do
  err_or_branches :: Either (Error, Env a c v) [Env a c v] <-
    start
      & (`runReaderT` mkCtx cfg)
      & (`execStateT` env0)
      & ListT.toList
      & (`evalStateT` cfg.initialGas)
      & runExceptT

  case err_or_branches of
    Left (err, env) -> return $ Left (err, env)
    Right branches -> return $ Right branches

start :: (Monad m, Ord a, Ord c, Ord v, Pretty a, Pretty c, Pretty v, Show a, Show c, Show v) => T a c v m ()
start = do
  -- initialize trace
  tell_traceGoals =<< gets activeGoals
  tell_traceGoals =<< gets suspendedGoals
  tell_traceGoals =<< gets failedGoals
  -- enter loop
  loop

loop :: forall a c v m. (Monad m, Ord a, Ord c, Ord v, Pretty a, Pretty c, Pretty v, Show a, Show c, Show v) => T a c v m ()
loop = do
  ctx <- ask

  -- update gas
  do
    g <- lift . lift $ get
    -- check gas
    when (g & isDepletedGas) do
      env <- get
      throwError (OutOfGas, env)
    -- update gas
    lift . lift $ modify decrementGas

  -- process next active goal
  extractNextActiveGoal >>= \case
    -- if there are no more active goals, then done and terminate this branch successfully
    Nothing -> do
      env <- get
      tellMsgs
        [ (Msg.mk 1 "--------------------------------")
            { Msg.contents =
                [ "status =" <+> "no more active goals",
                  "env =" <+> pPrint env
                ]
            }
        ]
    Just goal -> do
      do
        env <- get
        tellMsgs
          [ (Msg.mk 1 "--------------------------------")
              { Msg.contents =
                  [ "status =" <+> "processing goal:" <+> pPrint goal,
                    "env =" <+> pPrint env
                  ]
              }
          ]

      goal' <- liftT $ normAliasesInGoal ctx.config.exprAliases goal
      if ctx.config.shouldSuspend goal'
        then do
          tellMsgs
            [ (Msg.mk 1 "suspending goal")
                { Msg.contents =
                    [ "goal (before normAliasInGoal) =" <+> pPrint goal,
                      "goal (after normAliasInGoal)  =" <+> pPrint goal'
                    ]
                }
            ]
          -- TODO: what is the significance of putting the goal at the beginning or end of this list?
          modify \env -> env {suspendedGoals = goal' : env.suspendedGoals}
        else tryRules goal

      loop

tryRules :: (Monad m, Ord v, Ord c, Ord a, Pretty v, Pretty c, Pretty a, Eq c, Show v, Show c, Show a) => Goal a c v -> T a c v m ()
tryRules goal = do
  ctx <- ask

  rules' <-
    gets
      (\env -> if ctx.config.useIndexing then filterPathIndexing ctx.config.exprAliases goal env.pathIndex else env.rules)
      -- freshen each rule
      >>= traverse (runFreshening . Freshening.freshenRule)
      -- apply current substitution to each rule (to update any existential variables)
      >>= traverse (\rule -> gets \env -> substRule env.sigma rule)
      -- apply constrained ruleset option if present
      <&> case goal.goalOpts.constrainedRulesetGoalOpt of
        Nothing -> id
        Just ruleNames -> List.filter \rule -> rule.name `Set.member` ruleNames

  branches :: [(Rule a c v, Env a c v)] <- do
    env <- get
    rules'
      & filterMapM
        ( \rule ->
            rule
              & tryRule goal
              & (`runReaderT` ctx)
              & (`runStateT` env)
              & lift . lift
              & fmap \case
                (False, _) -> Nothing
                (True, env') -> Just (rule, env')
        )

  if null branches
    then do
      let step =
            FailureStep
              { goal = goal,
                reason = "no rules apply to this goal"
              }
      tell_traceStep step

      do
        env <- get
        tellMsgs
          [ (Msg.mk 1 $ "failed goal because:" <+> step.reason)
              { Msg.contents =
                  [ "goal =" <+> pPrint goal,
                    "env =" <+> pPrint env
                  ]
              }
          ]

      modify \env -> env {failedGoals = env.failedGoals <> [goal]}

      when
        ( or
            [ isAggressiveStrategy ctx.config.strategy,
              goal.goalOpts.requiredGoalOpt
            ]
        )
        do
          env <- get
          tellMsgs [(Msg.mk 1 "pruning branch since faild a required goal") {Msg.contents = ["goal =" <+> pPrint goal, "env =" <+> pPrint env]}]
          env.suspendedGoals <&>>= \goal' ->
            tell_traceStep FailureStep {goal = goal', reason = "failed other required goal:" <+> pPrint goal}
          reject
    else do
      let cutBranches =
            branches & filterMap \(rule, env) ->
              if rule.ruleOpts.cutRuleOpt
                then Just ((), env)
                else Nothing
      case null cutBranches of
        -- if any cut rules were successful, then prune all branches that used non-cut rules
        False -> fromBranches cutBranches
        True -> fromBranches (branches <&> \(_, env) -> ((), env))

-- | Returns a `Bool` indicating whether or not applying the rule was successful.
tryRule :: forall m a c v. (Monad m, Ord v, Pretty v, Pretty c, Pretty a, Eq a, Eq c, Show a, Show c, Show v) => Goal a c v -> Rule a c v -> T a c v m Bool
tryRule goal rule = do
  ctx <- ask
  tellMsgs
    [ (Msg.mk 3 "attempting to unify goal with rule's conclusion")
        { Msg.contents =
            [ "rule =" <+> pPrint rule.name,
              "goal =" <+> pPrint goal,
              "conc =" <+> pPrint rule.conc
            ]
        }
    ]

  env_beforeUnification <- get

  (err_or_goal', env_uni') <-
    lift . lift . lift . lift . lift . lift $
      ( do
          atom <- Unification.unifyAtom goal.atom rule.conc
          -- NOTE: it seems odd that this is required, since I _thought_ that in the implementation of unification it incrementally applies the substitution as it is computed (viz `Unification.setVarM`)
          Unification.normEnv
          return atom
      )
        & ( `runReaderT`
              Unification.Ctx
                { exprAliases = ctx.config.exprAliases
                }
          )
        & runExceptT
        & (`runStateT` Unification.emptyEnv)

  let sigma_uni :: Subst c v
      sigma_uni = env_uni' ^. Unification.sigma
  case err_or_goal' of
    Left err -> do
      tellMsgs
        [ (Msg.mk 2 "failed to unify goal with rule's conclusion")
            { Msg.contents =
                [ "rule =" <+> pPrint rule.name,
                  "err =" <+> pPrint err,
                  "show err =" <+> text (show err),
                  "sigma_uni =" <+> pPrint sigma_uni,
                  "goal =" <+> pPrint goal
                ]
            }
        ]
      return False
    Right goal' -> do
      tellMsgs
        [ (Msg.mk 1 "successfully unified goal with rule's conclusion")
            { Msg.contents =
                [ "rule =" <+> pPrint rule.name,
                  "sigma_uni =" <+> pPrint sigma_uni,
                  "goal =" <+> pPrint goal,
                  "goal' =" <+> pPrint goal'
                ]
            }
        ]

      case rule.ruleOpts.suspendRuleOpt of
        Just f | f goal -> do
          -- reset env to before unification with rule's head
          -- suspend this goal and constrain the set of rules that can apply to it to just this rule
          modify \env ->
            env_beforeUnification
              { suspendedGoals =
                  env.suspendedGoals
                    <> [ goal
                           { goalOpts =
                               goal.goalOpts
                                 { constrainedRulesetGoalOpt = Just (Set.fromList [rule.name]),
                                   requiredGoalOpt = True
                                 }
                           }
                       ]
              }
          tellMsgs
            [ (Msg.mk 1 "suspended goal for just this rule")
                { Msg.contents =
                    [ "rule =" <+> pPrint rule.name,
                      "goal =" <+> pPrint goal
                    ]
                }
            ]
          return True
        _ -> do
          subgoals <-
            fmap concat $
              rule.hyps <&>>= \case
                GoalHyp subgoal -> do
                  let subgoal' = subgoal & substGoal sigma_uni
                  return [subgoal']

          unless (null subgoals) do
            tellMsgs
              [ (Msg.mk 1 "new subgoals")
                  { Msg.contents = subgoals <&> pPrint
                  }
              ]

          suspendedGoals_old <- gets suspendedGoals

          do
            env <- get
            tellMsgs
              [ (Msg.mk 1 "new sigma")
                  { Msg.contents =
                      [ "old sigma =" <+> pPrint env.sigma,
                        "new sigma =" <+> pPrint (env.sigma & composeSubst_unsafe sigma_uni)
                      ]
                  }
              ]

          modify \env ->
            env
              { suspendedGoals =
                  substGoal sigma_uni
                    <$> env.suspendedGoals,
                activeGoals =
                  substGoal sigma_uni
                    <$> case ctx.config.strategy of
                      -- for a breadth first strategy, process old goals before new goals
                      BreadthFirstStrategy -> env.activeGoals <> subgoals
                      -- for a depth first strategy, process new goals before old goals
                      DepthFirstStrategy _opts -> subgoals <> env.activeGoals,
                sigma =
                  composeSubst_unsafe sigma_uni env.sigma
              }

          -- for each suspended goal that was refined by sigma_uni, make it active again
          suspendedGoals_curr <- gets suspendedGoals
          (activeGoals_resumed, suspendedGoals_new) <-
            zip suspendedGoals_old suspendedGoals_curr
              & foldM
                ( \(activeGoals_resumed, suspendedGoals_new) (suspendedGoal_old, suspendedGoal_curr) ->
                    if suspendedGoal_old == suspendedGoal_curr
                      then
                        -- if the suspended goal was NOT refined by the substitution, then leave it suspended
                        return (activeGoals_resumed, suspendedGoal_old : suspendedGoals_new)
                      else do
                        tell_traceStep
                          ResumeStep
                            { goal = suspendedGoal_curr,
                              reason = "the suspended goal was refined by a new substitution:" <+> pPrint sigma_uni
                            }
                        tellMsgs
                          [ (Msg.mk 2 "resume goal")
                              { Msg.contents = [pPrint suspendedGoal_curr]
                              }
                          ]
                        return (suspendedGoal_curr : activeGoals_resumed, suspendedGoals_new)
                )
                ([], [])
          modify \env ->
            env
              { activeGoals = activeGoals_resumed <> env.activeGoals,
                suspendedGoals = suspendedGoals_new
              }

          -- record step
          let step = ApplyRuleStep {goal, ruleName = rule.name, sigma = sigma_uni, subgoals}
          modify \env -> env {stepsRev = step : env.stepsRev}
          tell_traceStep step

          tellMsgs
            [ (Msg.mk 1 "successfully applied rule")
                { Msg.contents =
                    ["rule =" <+> pPrint rule.name]
                }
            ]

          return True

runFreshening :: (Monad m) => Freshening.M c v x -> T a c v m x
runFreshening m = do
  env_freshening <- do
    env <- get
    return
      Freshening.Env
        { sigma = emptySubst,
          freshCounter_vars = env.freshCounter_vars,
          freshCounter_goals = env.freshCounter_goals,
          existentialVars = Set.empty
        }
  let (x, env_freshening') = m `runState` env_freshening
  modify \env ->
    env
      { freshCounter_vars = env_freshening'.freshCounter_vars,
        freshCounter_goals = env_freshening'.freshCounter_goals
      }
  return x

-- | Nondeterministically choose from a list.
choose :: (Monad m) => [x] -> T a c v m x
choose = lift . lift . foldr cons mempty

-- | Nondeterministically pursue all branches.
fromBranches :: (Monad m) => [(x, Env a c v)] -> T a c v m x
fromBranches branches = lift $ StateT $ const $ foldr cons mempty branches

-- | Nondeterministically rejected branch.
reject :: (Monad m) => T a c v m x
reject = lift . lift $ mempty

extractNextActiveGoal :: (Monad m) => T a c v m (Maybe (Goal a c v))
extractNextActiveGoal =
  gets activeGoals >>= \case
    [] -> return Nothing
    goal : activeGoals' -> do
      modify \env -> env {activeGoals = activeGoals'}
      return $ Just goal

tellMsgs :: (Monad m) => [Msg] -> T a c v m ()
tellMsgs = lift . lift . lift . lift . lift . lift . Writer.tell

tell_traceStep :: (Monad m) => Step a c v -> T a c v m ()
tell_traceStep step =
  lift . lift . lift . Writer.tell $
    Trace
      { traceSteps = Map.singleton step.goal.goalIndex [step],
        traceGoals =
          Map.fromList
            [ (g.goalIndex, g)
              | g <- case step of
                  ApplyRuleStep {..} -> subgoals
                  FailureStep {} -> []
                  SuspendStep {} -> []
                  ResumeStep {} -> []
                  SolveStep {} -> []
            ]
      }

tell_traceGoals :: (Monad m) => [Goal a c v] -> T a c v m ()
tell_traceGoals goals =
  lift . lift . lift . Writer.tell $
    Trace
      { traceSteps = Map.empty,
        traceGoals = Map.fromList [(g.goalIndex, g) | g <- goals]
      }
