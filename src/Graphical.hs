--------------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Graphical where

import qualified Data.Set as Set

import Util
import DiagUtil
import Configuration

--------------------------------------------------------------------------------

varCol v
  | v == "0"  = white
  | otherwise = lightgrey

traceLegend vars t = (xlegend, ylegend)
  where xlegend = map xtext vars
        ylegend = [ ytext (kind e) | e <- traceEvents t ]

traceEventsDiag :: GridOptions -> [Var] -> Trace -> Diag
traceEventsDiag opts vars t =
  gridDiag opts (Grid elts xlegend ylegend)
  where
    (xlegend, ylegend) = traceLegend vars t
    elts = do
      e <-  traceEvents t
      return $ do
        x <- vars
        case (evEff e) `varValue` x of
          Just v -> return $ squareBullet # fc (varCol v)
          Nothing ->
            case (evPre e) `varValue` x of
              Just v  -> return $ circleBullet # fc (varCol v)
              Nothing -> return $ mempty

traceExecutionDiag :: GridOptions -> [Var] -> Trace -> PState -> Diag
traceExecutionDiag opts vars t i =
  gridDiag opts (Grid elts xlegend ylegend)
  where
    steps = init (scanTrace t i)
    (xlegend, ylegend) = traceLegend vars t
    elts = stateRows vars steps

traceFullDiag vars t i =
  traceEventsDiag def vars t ||| strutX 0.8 |||
  traceExecutionDiag (def {showYLegend = False}) vars t i

--------------------------------------------------------------------------------

stateRows vars steps = do
  s <- steps
  return $ do
    x <- vars
    case s of
      Bot -> error "bottom encountered"
      PVal pv ->
        case pv `varValue` x of
          Just v  -> return $ circleBullet # fc (varCol v)
          Nothing -> return $ mempty

--------------------------------------------------------------------------------

prefixesDiag prefs cfg =
  gridDiag def (Grid elts xlegend ylegend)
  where
    n = length prefs
    labels = topological cfg
    xlegend = [ xtext (take 3 . kind $ eventsMap cfg ! l) | l <- labels ]
    ylegend = replicate n mempty

    elts = do
      p <- prefs
      return $ do
        l <- labels
        if l `Set.member` p
          then return $ dotBullet
          else return $ mempty

residualsDiag vars cfg =
  prefsDiag ||| strutX 0.8 ||| resDiag
  where
    (prefs, res) = unzip $ init (resPres cfg)
    prefsDiag = prefixesDiag prefs cfg
    resDiag = gridDiag def (Grid elts xlegend ylegend)
      where
        elts = stateRows vars res
        xlegend = map xtext vars
        ylegend = replicate (length elts) mempty

--------------------------------------------------------------------------------
renderDiag f d =
  renderSVG f sizeSpec $ pad 1.1 (strutX 2 ||| d)
  where sizeSpec = mkSizeSpec (Just 400.0 ^& Nothing)

dumpTrace :: FilePath -> [Var] -> Trace -> IO ()
dumpTrace f v t = renderDiag f (traceEventsDiag def v t)

dumpExecution :: FilePath -> [Var] -> Trace -> PState -> IO ()
dumpExecution f v t i = renderDiag f (traceFullDiag v t i)

dumpTrace' :: FilePath -> Trace -> IO ()
dumpTrace' f t = dumpTrace f (traceVariablesList t) t

dumpExecution' :: FilePath -> Trace -> PState -> IO ()
dumpExecution' f t i = dumpExecution f (traceVariablesList t) t i

dumpResiduals :: FilePath -> [Var] -> Configuration -> IO ()
dumpResiduals f vars cfg = renderDiag f (residualsDiag vars cfg)

dumpResiduals' :: FilePath -> Configuration -> IO ()
dumpResiduals' f cfg = dumpResiduals f (configVariablesList cfg) cfg

--------------------------------------------------------------------------------

