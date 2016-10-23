--------------------------------------------------------------------------------

{-# LANGUAGE TupleSections #-}

module Shortcuts
  ( module Shortcuts,
    module Configuration
  ) where

import qualified Data.Map as Map

import Util
import Configuration

--------------------------------------------------------------------------------

true  = "1"
false = "0"

named :: Event -> Kind -> Event
named e n = e { kind = n }

modify :: Var -> Val -> Val -> Event
modify x v v' = Event "" (pval [(x, v)]) (pval [(x, v')])

activate :: Var -> Event
activate x = modify x false true

desactivate :: Var -> Event
desactivate x = modify x true false

when :: Event -> [(Var, Val)] -> Event
when e cs = e { evPre = pval cs `conj` evPre e }

doNothing :: Event
doNothing = Event "" (pval []) (pval [])

initFalse ::[Var] -> PState
initFalse vs = pstate (map (,false) vs)

labelAuto :: [Event] -> [LabeledEvent]
labelAuto evs =
  aux evs Map.empty
  where numsuf s i = s ++ "-" ++ show i
        aux [] _ = []
        aux (e:es) alreadyGiven =
          let l = kind e in
            case Map.lookup l alreadyGiven of
              Nothing ->
                (l, e) : aux es (Map.insert l 1 alreadyGiven)
              Just n ->
                let l' = numsuf l (n + 1) in
                  (l', e) : aux es (Map.insert l (n+1) alreadyGiven)

mkTrace' :: [Event] -> Trace
mkTrace' = Trace . labelAuto

mkConfig' :: [Event] -> Configuration
mkConfig' = configFromTrace . mkTrace'

--------------------------------------------------------------------------------
