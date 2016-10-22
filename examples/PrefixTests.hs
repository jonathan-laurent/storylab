module PrefixTests where

import Shortcuts


dumb s = (s, doNothing `named` s)

evs = [dumb "a", dumb "b", dumb "c", dumb "d", dumb "e", dumb "f"]

printPrefixes ord =
  mapM_ printPrefix (prefixes (mkConfig evs ord))
  where
    printPrefix p =
      putStrLn (concat (Set.toList p))

printNumPrefixes ord =
  print (length (prefixes (mkConfig evs ord)))

        
ord1 = [("a", "b"), ("a", "c"), ("c", "d"), ("c", "e"),
       ("b", "f"), ("d", "f"), ("e", "f")]
       
tp1 = printPrefixes ord1

ord2 = []

tp2 = printNumPrefixes ord2

ord3 = [("a", "b"), ("b", "c"), ("c", "d"), ("d", "e"), ("e", "f")]
    
tp3 = printPrefixes ord3
