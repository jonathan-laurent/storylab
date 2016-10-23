--------------------------------------------------------------------------------

module Trace
  ( module Trace,
    module Event
  )where

import qualified Data.Set as Set

import Util
import Event

--------------------------------------------------------------------------------

type Label  = String

type LabeledEvent = (Label, Event)

data Trace = Trace [LabeledEvent]

instance Show Trace where
  show (Trace t) = intercalate " -> " (map (kind . snd) t)

--------------------------------------------------------------------------------

instance EventLike Trace where
  pre t = foldr rewind top (traceEvents t)

  -- We have to enforce the invariant `pre(t) inter eff(t) = empty`
  eff t =
    let e = foldl update top (map eff (traceEvents t)) in
      case (pre t) of
        Bot -> e
        PVal pv -> e `minus` pv

traceEvents :: Trace -> [Event]
traceEvents (Trace t) = map snd t

traceVariables :: Trace -> Set Var
traceVariables (Trace t) =
  Set.unions [ supp (evPre e) | (l, e) <- t ]

traceVariablesList :: Trace -> [Var]
traceVariablesList = Set.toList . traceVariables

scanTrace :: Trace -> PState -> [PState]
scanTrace t p = scanl (flip apply) p  (traceEvents t)

--------------------------------------------------------------------------------
