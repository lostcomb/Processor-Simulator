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
  -- Counts the number of instructions executed.
  , _issInsts              :: Int
  , _wrbInsts              :: Int
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
  , _issInsts              = 0
  , _wrbInsts              = 0
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
toString d = "Cycles: "               ++ show (_cycles                d)              ++ "\n" ++
             "Issued Instructions: "  ++ show (_issInsts              d)              ++ "\n" ++
             "Written Instructions: " ++ show (_wrbInsts              d)              ++ "\n" ++
             "Stalled Counts: (F, "   ++ show (_fetchStalledCount     d) ++ ") "      ++
                             "(D, "   ++ show (_decodeStalledCount    d) ++ ") "      ++
                             "(R, "   ++ show (_robStalledCount       d) ++ ") "      ++
                             "(I, "   ++ show (_issueStalledCount     d) ++ ") "      ++
                             "(E, "   ++ show (_executeStalledCount   d) ++ ") "      ++
                             "(W, "   ++ show (_writebackStalledCount d) ++ ")"       ++ "\n" ++
             "Branch predictions: "   ++ show (_hitPredictions        d) ++ " hits, " ++
                                         show (_misPredictions        d) ++ " misses"

-- |This function returns the issue rate that the simulation data defines.
issueRate :: Simdata -> Float
issueRate d = fromIntegral (_issInsts d) / fromIntegral (_cycles d)

-- |This function returns the writeback rate that the simulation data defines.
writebackRate :: Simdata -> Float
writebackRate d = fromIntegral (_wrbInsts d) / fromIntegral (_cycles d)

-- |This function returns teh branch prediction rate that the simulation data
--  defines.
branchPredictionRate :: Simdata -> Float
branchPredictionRate d = fromIntegral (_hitPredictions d) / fromIntegral (length $ _predictions d)
