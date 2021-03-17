module Prolog.Database
    ( Database
    , from, clauses, member
    , asserta, assertz
    , retract
    ) where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (isJust, fromMaybe)
import Data.List (find, delete)

import Prolog.Syntax ( Signature, Program, Clause(..), Term, signature ) 
import Prolog.Unification.Unify

type Database = Map Signature [Clause]

from :: Program -> Database
from = foldr (\clause@(t :- _) -> Map.insertWith (++) (signature t) [clause])
        Map.empty

clauses :: Term -> Database -> [Clause]
clauses t = fromMaybe [] . Map.lookup (signature t)

member :: Term -> Database -> Bool
member = Map.member . signature


-- | Assert a clause into the database. The predicate asserta/1 asserts the clause as the 
-- | first clause of the predicate, while assertz/1 asserts the clause as the last clause. 
asserta, assertz :: Clause -> Database -> Database
asserta clause@(t :- _) = Map.insertWith (++) (signature t) [clause]
assertz clause@(t :- _) = Map.insertWith (flip (++)) (signature t) [clause]

-- | The retract/1 predicate (denoted retract(+Clause)) removes the first unifying clause
-- | in the database. 
retract :: Clause -> Database -> Maybe Database
retract clause@(t :- _) database = do
    let sig = signature t
    cs <- Map.lookup sig database
    c <- find (isJust . mgu False clause) cs        -- find the first unifying clause in the set of clauses for the given clause
    return $ Map.insert sig (delete c cs) database  -- remove the unifying clause from the database, performed by deleting it from the set of clauses and re-inserting the clauseset
                                                    -- TODO: Is there a more efficient way?






