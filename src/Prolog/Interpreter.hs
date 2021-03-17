{-# LANGUAGE LambdaCase #-}
module Prolog.Interpreter
    ( Branch(..), ChoicePoint(..), Stack
    , InterpreterState, InterpreterError
    , Interpreter, runInterpreter
    ) where

import Control.Monad.Trans.RWS.Strict
import Control.Monad.Except
import Control.Applicative ((<$>))

import Data.Maybe (isJust, maybeToList)
import Data.Generics (everywhere, mkT)

import Prolog.Syntax

import Prolog.Database as Database
import Prolog.Unification.Substitution as Substitution
import Prolog.Unification.Unify


-------------------------------------------------------------------------------
-- Interpreter State
-------------------------------------------------------------------------------

data Branch =
    Branch
        { b_unifier   :: Substitution
        , b_goals     :: [Term]
        }
    deriving (Eq, Show)

data ChoicePoint =
    ChoicePoint
        { cp_unifier   :: Substitution
        , cp_goals     :: [Term]
        , cp_branches  :: [Branch]
        }
    deriving (Eq, Show)

type Stack = [ChoicePoint]

data InterpreterState =
    InterpreterState
        { s_depth     :: Int
        , s_stack     :: Stack
        , s_unifier   :: Substitution
        }
    deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Interpreter Monad
-------------------------------------------------------------------------------

data InterpreterError = ClauseNotFound Signature
    deriving (Eq, Show)

-- TODO: Implement events for execution logging
type InterpreterEvent = ()
type Interpreter a =
    (RWST
        Database            -- Reading a database 
        [InterpreterEvent]  -- Logging interpreter events e.g. enter, exit, fail, cut, etc
        InterpreterState    -- Writing to stack, depth, and current cp_unifier. 
        (Except
            InterpreterError)
        a)

initInterpreter :: InterpreterState
initInterpreter = InterpreterState 1 [] empty

runInterpreter :: Program -> [Term] -> Either InterpreterError ([Substitution], InterpreterState, [InterpreterEvent])
runInterpreter program goals = runExcept $ runRWST (resolve goals) (Database.from program) initInterpreter

-------------------------------------------------------------------------------
-- Interpreter Monad State Mutators / Accessors
-------------------------------------------------------------------------------

putUnifier :: Substitution -> Interpreter ()
putUnifier u = modify $ \s -> s{ s_unifier = u }

popChoicePoint :: Interpreter (Maybe ChoicePoint)
popChoicePoint = gets s_stack >>= \case
    []          -> return Nothing
    cp : cps    -> do
        modify $ \s@InterpreterState{ s_depth = d } -> s{ s_stack = cps, s_depth = d - 1 }
        return $ Just cp

pushChoicePoint :: ChoicePoint -> Interpreter ()
pushChoicePoint cp = modify $ \s@InterpreterState{ s_stack = cps, s_depth = d } -> s{ s_stack = cp : cps, s_depth = d + 1 }

getClauses :: Term -> Interpreter [Clause]
getClauses t = asks (clauses t)

-------------------------------------------------------------------------------
-- Interpreter Semantics
-------------------------------------------------------------------------------

-- | Branch g gs determines the branches for the clause g
-- | with remaining clauses gs.  
branch :: Term -> [Term] -> Interpreter [Branch]
branch g gs = do
        cs <- getClauses g
        s <- gets s_unifier
        cs' <- mapM rename cs
        return $ branchesOf s cs'
    where
        branchesOf :: Substitution -> [Clause] -> [Branch]
        branchesOf s cs = do
            t :- ts <- cs
            s' <- mgu False (apply s g) t                   -- Attempt to unify the lhs of the clause and the goal
            return $ Branch s (cutT $ apply s (ts ++ gs))   -- Return the corresponding cp_unifier and new goal clauses (w/ transformed cuts)

        -- | This transformation increments the cut depth of each cut
        -- | in the set of clauses cs. 
        cutT = everywhere $ mkT $ \(Cut n) -> Cut (n + 1)


-- | Rename c i renames the depth of all variables in the list of clauses to 
-- | depth i. This is used when expanding goal clauses. 
-- |
-- | Uses some fancy Haskell. Note to future self: everywhere :: (forall a. Data a => a -> a) -> (forall. Data a => a -> a)
-- | applies a transformation recursively to the next argument. 
-- | mkT :: (Typeable a, Typeable b) => (b -> b) -> a -> a makes a generic transformation. 
rename :: Clause -> Interpreter Clause
rename c = do
    d <- gets s_depth
    return $ (everywhere $ mkT $ \(Variable lid _) -> Variable lid d) c

-- | cut n removes the n most recent choicepoints from the stack. This is as described in 
-- | lectures. Note that the current depth of interpreter *is not* modified. 
cut :: Int -> Interpreter ()
cut n = modify $ \s@InterpreterState{ s_stack = cps } -> s{ s_stack = take n cps }

-- | backtrack takes the current choicepoint, and then considers other branches
-- | in the choicepoint. 
backtrack :: Interpreter [Substitution]
backtrack = popChoicePoint >>= \case
    Nothing -> return [] -- If we have nothing to backtrack to, then we have no solution, which corresponds to no unifiers
    Just ChoicePoint{ cp_unifier=u, cp_goals=gs, cp_branches=bs} -> do
        putUnifier u  -- Backtrack to the cp_unifier we had when we created the choicepoint 
        choose gs bs

-- | choose gs bs chooses a branch b from bs w/ current goals gs 
-- | and explores it, pushing the choicepoint onto the stack. 
choose :: [Term] -> [Branch] -> Interpreter [Substitution]
choose _ [] = backtrack
choose gs (Branch{ b_unifier=u', b_goals=gs' } : bs) = do
    u <- gets s_unifier
    pushChoicePoint $ ChoicePoint u gs bs   -- The choicepoint consists of the current cp_unifier, goals, and remaining branches
    -- Now swap unifiers w/ the cp_unifier of the branch and resolve
    putUnifier u'
    resolve gs'

resolve :: [Term] -> Interpreter [Substitution]
resolve = \case
    [] -> do
        s <- gets s_unifier
        (s:) <$> backtrack
    Cut n : gs -> do
        cut n
        resolve gs
    Asserta c : gs ->
        local (asserta c) $ resolve gs
    Assertz c : gs ->
        local (assertz c) $ resolve gs
    Retract c : gs -> do
        db <- asks (retract c)
        case db of
            Nothing -> backtrack  -- If retract fails, then backtrack since the goal has failed
            Just db -> local (const db) $ resolve gs
    g : gs -> do
        db <- ask
        unless (g `member` db) $ throwError (ClauseNotFound $ signature g)
        bs <- branch g gs
        choose gs bs


