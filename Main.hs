--------------------------------------------------------------------------------

module Main where

import Configuration
import Shortcuts
import Graphical

--------------------------------------------------------------------------------

x  = "x"
b  = "b"
kx = "kx"

init = initFalse [x, b, kx]

bind = activate b `named` "b"

unbindSlow = desactivate b `when` [(kx, true)]  `named` "u*"

unbindFast = desactivate b `when` [(kx, false)] `named` "u"

phos = activate x `when` [(b, true)] `named` "p"

eoi = doNothing `when` [(x, true), (b, false)] `named` "eoi"

phosK = activate kx `named` "pK"

unphosK = desactivate kx `named` "uK"

--------------------------------------------------------------------------------

-- t = mkTrace' [bind, phos, unbindFast, bind, unbindFast, eoi]

t = mkTrace' [phosK, bind, phos, unbindSlow, eoi]

c = configFromTrace t

main = do
  dumpTrace'     "trace.svg"      t
  dumpResiduals' "residuals.svg"  c
  dumpExecution' "execution.svg"  t (pre t)

--------------------------------------------------------------------------------
