module Prolog.Unification.Substitution
    ( Substitution, Substitutable(..)
    , empty, singleton
    , (@->), (@@)
    ) where

import Data.List (lookup, nub, concatMap)

import Prolog.Syntax (Variable, Term(..))


type Substitution = [(Variable, Term)]

empty :: Substitution
empty = []

infix 6 @->
(@->) :: Variable -> Term -> Substitution
x @-> t = [(x, t)]

singleton :: Variable -> Term -> Substitution
singleton = (@->)

infixr 6 @@
(@@) :: Substitution -> Substitution -> Substitution
s1 @@ s2 = [ (x, apply s1 t) | (x, t) <- s2 ] ++ s1


class Substitutable t where
    apply :: Substitution -> t -> t
    fvs   :: t -> [Variable]

instance Substitutable Term where
    
    apply s (Composite lid ts) = Composite lid (apply s ts)
    apply s (Var x) = case lookup x s of
        Just t -> t
        Nothing -> Var x
    apply s t = t
    

    fvs (Composite _ ts) = fvs ts
    fvs (Var x) = [x]
    fvs _ = []

instance Substitutable a => Substitutable [a] where

    apply s = map (apply s)
    fvs = nub . concatMap fvs


