{-# LANGUAGE TemplateHaskell,
             MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleInstances,
             RankNTypes #-}
module Simulator.Data.Simdata
  ( module Simulator.Data.Simdata
  ) where

import Data.Int
import Control.Lens
import Simulator.Data.Instruction

-- |This data type defines the data to be collected during simulation.
data Simdata = Simdata
  -- Counts the number of cycles executed.
  { _cycles                :: Int
  -- Counts the number of instrctions executed.
  , _insts                 :: Int
  -- Counts the number of instructions executed in each cycle during execution.
  , _instsPerCycle         :: [ Int ]
  -- Counts the number of cycles each stage is stalled during execution.
  , _fetchStalledCount     :: Int
  , _decodeStalledCount    :: Int
  , _robStalledCount       :: Int
  , _issueStalledCount     :: Int
  , _executeStalledCount   :: Int
  , _writebackStalledCount :: Int
  -- Counts the number of correct and incorrect branch predictions.
  , _predictions           :: [ (Inst Int32, Bool) ]
  , _misPredictions        :: Int
  , _hitPredictions        :: Int
  -- Counts the number of instructions executed out of order in each cycle
  -- during execution.
  , _outOfOrderPerCycle    :: [ Int ]
  }

-- Let Template Haskell make the lenses for Simdata.
makeLenses ''Simdata

-- |This defines the simulation data before simulation has begun.
newSimdata :: Simdata
newSimdata = Simdata
  { _cycles                = 0
  , _insts                 = 0
  , _instsPerCycle         = []
  , _fetchStalledCount     = 0
  , _decodeStalledCount    = 0
  , _robStalledCount       = 0
  , _issueStalledCount     = 0
  , _executeStalledCount   = 0
  , _writebackStalledCount = 0
  , _predictions           = []
  , _misPredictions        = 0
  , _hitPredictions        = 0
  , _outOfOrderPerCycle    = []
  }

-- |This function returns the stringular representation of the simulation data.
toString :: Simdata -> String
toString d = "Cycles: "             ++ show (_cycles                d)              ++ "\n" ++
             "Instructions: "       ++ show (_insts                 d)              ++ "\n" ++
             "Stalled Counts: (F, " ++ show (_fetchStalledCount     d) ++ ") "      ++
                             "(D, " ++ show (_decodeStalledCount    d) ++ ") "      ++
                             "(R, " ++ show (_robStalledCount       d) ++ ") "      ++
                             "(I, " ++ show (_issueStalledCount     d) ++ ") "      ++
                             "(E, " ++ show (_executeStalledCount   d) ++ ") "      ++
                             "(W, " ++ show (_writebackStalledCount d) ++ ")"       ++ "\n" ++
             "Branch predictions: " ++ show (_hitPredictions        d) ++ " hits, " ++
                                       show (_misPredictions        d) ++ " misses"

-- |This function returns the issue rate that the simulation data defines.
issueRate :: Simdata -> Float
issueRate d = fromIntegral (_insts d) / fromIntegral (_cycles d)
