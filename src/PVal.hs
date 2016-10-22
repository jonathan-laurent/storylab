{-# LANGUAGE TupleSections #-}

module PVal
  ( Var,
    Val,
    PVal (..),
    StatePredicate
    ( top,
      conj,
      implies,
      isBot,
      update,
      incompatible,
      compatible,
      fromPVal,
      incompatibleWith
    ),
    pval,
    constrs,
    supp,
    without,
    minus,
    varValue,
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Util

--------------------------------------------------------------------------------

type Var    = String
type Val    = String

-- Invariant: each set in the map has at least one element
data PVal = Constrs (Map Var (Set Val)) deriving (Eq)

pval :: [(Var, Val)] -> PVal
pval l = Constrs (Map.fromList [(x, Set.singleton v) | (x, v) <- l])

constrs :: PVal -> [(Var, Val)]
constrs (Constrs cs) =
  concat [ map (x,) (Set.toList vs) | (x, vs) <- Map.toList cs ]

--------------------------------------------------------------------------------

supp :: PVal -> Set Var
supp (Constrs cs) = Map.keysSet cs

without :: PVal -> Set Var -> PVal
without (Constrs cs) xs =
  -- Implementation for the latest containers package
  -- Constrs (cs `Map.withoutKeys` xs)
  Constrs (Set.fold Map.delete cs xs)

varValue :: PVal -> Var -> Maybe Val
varValue (Constrs cs) x = do
  vs <- Map.lookup x cs
  return (Set.findMin vs)

removeConstr (Constrs cs) (x, v) = Constrs $
  case Map.lookup x cs of
    Nothing -> cs
    Just vs ->
      -- We have to maintain the invariant
      let vs' = Set.delete v vs in
        if Set.null vs' then Map.delete x cs
        else Map.insert x vs'  cs

minus :: PVal -> PVal -> PVal
minus pv pv' =
  foldl removeConstr pv (constrs pv')
  
--------------------------------------------------------------------------------

class StatePredicate p where
  top      :: p
  conj     :: p -> p -> p
  implies  :: p -> p -> Bool
  isBot    :: p -> Bool
  update   :: p -> PVal -> p
  
  incompatible :: p -> p -> Bool
  incompatible p p' = isBot (p `conj` p')

  compatible :: p -> p -> Bool
  compatible p p' = not (incompatible p p')

  fromPVal :: PVal -> p
  fromPVal pv = top `update` pv

  incompatibleWith :: p -> PVal -> Bool
  p `incompatibleWith` pv = p `incompatible` (fromPVal pv)

--------------------------------------------------------------------------------

instance StatePredicate PVal where
  
  top = Constrs Map.empty

  conj (Constrs cs) (Constrs cs') =
    Constrs (Map.unionWith Set.union cs' cs)

  implies (Constrs cs) (Constrs cs') =
    Map.isSubmapOfBy (Set.isSubsetOf) cs' cs

  isBot (Constrs cs) =
    any (\vs -> Set.size vs >= 2) (Map.elems cs)

  update pv1 pv2 = (pv1 `without` (supp pv2)) `conj` pv2

--------------------------------------------------------------------------------

instance Show PVal where
  show pv = intercalate ", " (map showConstr $ constrs pv)
    where
      showConstr (x, v) = x ++ "=" ++ v

--------------------------------------------------------------------------------
