{-# LANGUAGE DeriveDataTypeable #-}
module Prolog.Syntax 
    ( Term(..), Variable(..), var 
    , Clause(..)
    , Signature(..), signature
    , Program
    ) where

import Data.Generics (Data(..), Typeable(..))

-- TODO: Move identifiers to frontend syntax (for parser)
type LowerIdentifier = String
type UpperIdentifier = String

-- ^ The constructors [Composite] and [Var] defined the 
-- ^ Term Algebra T_V(O) where V is the set of variables
-- ^ defined by the type Variable and O is the set of operators
-- ^ defined by the LowerIdentifier type (at runtime these are checked). 
data Term 
    = Composite LowerIdentifier [Term] 
    | Var Variable

    -- ^ The wildcard _ term is optimized variable. 
    -- ^ Denoting that the variable is not used anywhere. 
    | Wildcard

    -- ^ The constructors influence the behavior of the Prolog interpreter.
    -- ^ Cut predicate !/0 changes the search behavior of the interpreter by removing branches (choicepoints)
    | Cut Int
    
    -- ^ The assert/1 and retract/1 class of the predicates dynamically modify the underlying database used by the 
    -- ^ interpreter at runtime. 
    | Asserta Clause
    | Assertz Clause
    | Retract Clause

    deriving (Show, Eq, Typeable, Data)



-- A variable is a UpperIdentifier X 
-- (assocates w/ some instance identifier n, denoted X_n)
data Variable = Variable UpperIdentifier Int 
    deriving (Eq, Typeable, Data)

instance Show Variable where
    show (Variable lid n) = show lid ++ "_" ++ show n

var :: String -> Variable
var = flip Variable 0


infix 6 :-
data Clause = Term :- [Term]
    deriving (Show, Eq, Typeable, Data)


type Program = [Clause]


-- Represents a rule / fact's signature
-- 
data Signature = Signature LowerIdentifier Int
    deriving (Eq, Ord)

instance Show Signature where
    show (Signature id arity) = id ++ "/" ++ show arity


signature :: Term -> Signature
signature (Composite id ts) = Signature id (length ts)
signature _ = error "Cannot obtain signature of non-composite term"



-- TODO: Move to interpreter (frontend)
-- data Command  
--     = Fact Clause
--     | Query [Term]
--     deriving (Show, Eq)


-- TODO: Define operators w/ precedence
-- data Operator = 
--     | Prefix String
--     | Infix Assoc String

-- data Assoc = 
--     | AssocLeft
--     | AssocRight

