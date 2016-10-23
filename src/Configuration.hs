--------------------------------------------------------------------------------

module Configuration
  ( module Configuration,
    module Trace,
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Graph as Graph

import Data.Graph (Graph, Vertex)

import Util
import Trace

--------------------------------------------------------------------------------

data Configuration = Conf {
  
  -- Core information
  graph       :: Graph ,
  fromLabel   :: Label -> Vertex ,
  toLabel     :: Vertex -> Label ,
  eventsMap   :: Map Label Event ,
  
  -- Cached informations
  topological :: [Label] ,
  labelsSet   :: Set Label ,
  predGraph   :: Graph
  }

--------------------------------------------------------------------------------

mkConfig :: [LabeledEvent] -> [(Label, Label)] -> Configuration
mkConfig les prec =
  let eMap :: Map Label Event
      eMap = Map.fromList les
      
      succMap :: Map Label [Label]
      succMap = Map.fromList . factorFst . groupBy ((==) `on` fst) $ prec

      gBuilder :: [(Event, Label, [Label])]
      gBuilder = [ (e, l, Map.findWithDefault [] l succMap) | (l, e) <- les ]

      -- (Graph, Vertex -> (node, key, [key]), key -> Maybe Vertex)
      (graph, vertexDescr, fromLabelSafe) = Graph.graphFromEdges gBuilder

      fromLabel l = case fromLabelSafe l of
                      Nothing -> error ("Label " ++ l ++ " does not exist")
                      Just v -> v

      toLabel v   = snd3 (vertexDescr v)
      topological = map toLabel (Graph.topSort graph)
      labelsSet   = Set.fromList topological
      predGraph   = Graph.transposeG graph
  in
    Conf graph fromLabel toLabel eMap topological labelsSet predGraph
  where
    factorFst ll = map (\l -> (fst (head l), map snd l)) ll
    fst3 (a, b, c) = a
    snd3 (a, b, c) = b

--------------------------------------------------------------------------------

reachable :: Graph -> Configuration -> Label -> [Label]
reachable g conf l =
   map (toLabel conf) $ Graph.reachable g (fromLabel conf l)

successors :: Configuration -> Label -> [Label]
successors conf = reachable (graph conf) conf
 
predecessors :: Configuration -> Label -> [Label]
predecessors conf = reachable (predGraph conf) conf

configEvents :: Configuration -> [Label]
configEvents = topological

reachMap graphF cfg =
  Map.fromList $ do
    l <- configEvents cfg
    let v = fromLabel cfg l
    let predsv = Graph.reachable (graphF cfg) v
    let preds = Set.fromList (map (toLabel cfg) predsv)
    return (l, preds)

predsMap :: Configuration -> Map Label (Set Label)
predsMap cfg = reachMap predGraph cfg

succsMap :: Configuration -> Map Label (Set Label)
succsMap cfg = reachMap graph cfg

--------------------------------------------------------------------------------

type Prefix = Set Label

prefixes :: Configuration -> [Prefix]
prefixes cfg =
  aux (topological cfg) Set.empty Set.empty
  where
    preds = predsMap cfg
    succs = succsMap cfg

    -- [aux opened inside outside] gives the set of all prefixes
    -- included in [opened] that contain every element of [inside]
    -- and none of [outside]
  
    aux [] inside outside =
      if Set.null (inside `Set.intersection` outside) then
        [Set.empty]
      else
        []
    
    aux (e : es) inside outside =
      withoutE ++ withE
      where
        withE =
          if e `Set.member` outside then []
          else
            let prefs = aux es (preds ! e `Set.union` inside) outside in
              map (Set.insert e) prefs
            
        withoutE =
          if e `Set.member` inside then []
          else
            aux es inside (succs ! e `Set.union` outside)
            

subtrace :: Configuration -> Set Label -> Trace
subtrace cfg keep =
  Trace [ (l, eventsMap cfg ! l) | l <- topological cfg, l `Set.member` keep ]

residual :: Configuration -> Prefix -> Set Label
residual cfg pr = (labelsSet cfg) `Set.difference` pr

resPres :: Configuration -> [(Prefix, PState)]
resPres cfg = do
  pref <- prefixes cfg
  let resT = subtrace cfg (residual cfg pref)
  return (pref, pre resT)

traceOfConfiguration :: Configuration -> Trace
traceOfConfiguration cfg = subtrace cfg (labelsSet cfg)

configVariablesList :: Configuration -> [Var]
configVariablesList  = traceVariablesList . traceOfConfiguration

--------------------------------------------------------------------------------

-- Config generation

-- Returns the set of ordered pairs (x_i, x_j) for (i < j)
triangle :: [a] -> [(a, a)]
triangle t =
  let t' = zip [1..] t in
    [ (x, y) | (ix, x) <- t', (iy, y) <- t', ix < iy ]

configFromTrace :: Trace -> Configuration
configFromTrace (Trace t) =
  mkConfig t prec
  where
    prec = [ (l1, l2)  |
             ((l1, e1), (l2, e2)) <- triangle t, not (concurrent e1 e2) ]

--------------------------------------------------------------------------------
