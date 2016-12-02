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

newSimdata :: Simdata
newSimdata = Simdata
  { _cycles                = 0
  , _insts                 = 0
  , _instsPerCycle         = []
  , _fetchStalledCount     = 0
  , _decodeStalledCount    = 0
  , _issueStalledCount     = 0
  , _executeStalledCount   = 0
  , _writebackStalledCount = 0
  , _predictions           = []
  , _misPredictions        = 0
  , _hitPredictions        = 0
  , _outOfOrderPerCycle    = []
  }
