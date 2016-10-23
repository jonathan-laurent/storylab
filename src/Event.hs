--------------------------------------------------------------------------------

module Event
  ( module Event,
    module PState,
    module PVal
  ) where

import qualified Data.Set as Set

import Util
import PVal
import PState

--------------------------------------------------------------------------------

class EventLike e where
  pre :: e -> PState
  eff :: e -> PVal
  
  apply :: e -> PState -> PState
  apply e p = p `update` (eff e)

  post :: e -> PState
  post e = apply e (pre e)

  rewind :: e -> PState -> PState
  rewind e p
    | p `incompatibleWith` (eff e) = bot
    | otherwise =
        case p of
          Bot -> Bot
          PVal pv -> (pre e) `conj` (fromPVal $ pv `without` (supp $ eff e))

--------------------------------------------------------------------------------

type Kind = String

data Event = Event { kind :: Kind , evPre :: PVal , evEff :: PVal }

instance EventLike Event where
  pre = fromPVal . evPre
  eff = evEff

instance Show Event where
  show e = kind e

--------------------------------------------------------------------------------

nonTriviallyConcurrent e1 e2 = all (== True)
  [ Set.null ((supp (evPre e1)) `Set.intersection` (supp (evEff e2))), 
    Set.null ((supp (evPre e2)) `Set.intersection` (supp (evEff e1))),
    evPre e1 `compatible` evPre e2,
    evEff e1 `compatible` evEff e2
  ]

triviallyConcurrent e1 e2 = all (== True)
  [ evPre e1 `incompatible` evPre e2,
    post e1 `incompatible` pre e2,
    post e2 `incompatible` pre e1
  ]

concurrent e1 e2 = triviallyConcurrent e1 e2 || nonTriviallyConcurrent e1 e2

--------------------------------------------------------------------------------
