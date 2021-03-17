module Prolog.Unification.Unify 
    (Unify(..)) where

import Control.Monad (MonadPlus, mzero)

import Prolog.Syntax ( Clause(..), Variable, Term(..) )

import Prolog.Unification.Substitution ((@->), (@@), Substitution, Substitutable(..)) 
import qualified Prolog.Unification.Substitution as Substitution


class Unify a where
    mgu :: MonadPlus m => Bool -> a -> a -> m Substitution

instance Unify Term where

    mgu withOccursCheck (Composite x1 ts1) (Composite x2 ts2) 
        | x1 == x2 && length ts1 == length ts2 = mgu withOccursCheck ts1 ts2
        
    mgu withOccursCheck (Var x) t = varBind withOccursCheck x t
    mgu withOccursCheck t (Var x) = varBind withOccursCheck x t
    mgu _ _ _  = mzero

varBind :: MonadPlus m => Bool -> Variable -> Term -> m Substitution
varBind withOccursCheck x t 
    | t == Var x                        = return Substitution.empty
    | x `elem` fvs t && withOccursCheck = mzero
    | otherwise                         = return (x @-> t)

instance Unify Clause where

    mgu withOccursCheck (t1 :- ts1) (t2 :- ts2) = 
        do s1 <- mgu withOccursCheck t1 t2
           s2 <- mgu withOccursCheck (apply s1 ts1) (apply s1 ts2)
           return (s2 @@ s1)

instance (Unify a, Substitutable a) => Unify [a] where

    mgu withOccursCheck (x:xs) (y:ys) = 
        do s1 <- mgu withOccursCheck x y
           s2 <- mgu withOccursCheck (apply s1 xs) (apply s1 ys)
           return (s2 @@ s1) 
    mgu _ [] [] = return Substitution.empty 
    mgu _ _ _ = mzero


