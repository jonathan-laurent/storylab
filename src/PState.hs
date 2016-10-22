--------------------------------------------------------------------------------

module PState where

import Util
import PVal

data PState = Bot | PVal PVal

--------------------------------------------------------------------------------

bot :: PState
bot = Bot

pstate :: [(Var, Val)] -> PState
pstate cs =
  let pv = pval cs in
    if isBot pv then Bot else PVal pv

--------------------------------------------------------------------------------

instance StatePredicate PState where

  top = PVal top

  conj Bot _ = Bot
  conj _ Bot = Bot
  conj (PVal pv) (PVal pv') = PVal (conj pv pv')

  implies Bot Bot = True
  implies Bot _   = True
  implies p Bot   = isBot p
  implies (PVal pv) (PVal pv') = implies pv pv'

  isBot Bot = True
  isBot (PVal pv) = isBot pv

  update Bot pv = Bot
  update (PVal pv1) pv2 = PVal (update pv1 pv2)


instance Show PState where
  show (PVal pv) = show pv
  show (Bot) = "bottom"

--------------------------------------------------------------------------------
