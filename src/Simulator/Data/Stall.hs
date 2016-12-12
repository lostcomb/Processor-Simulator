{-# LANGUAGE TemplateHaskell,
             MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleInstances,
             RankNTypes #-}
module Simulator.Data.Stall
  ( module Simulator.Data.Stall
  ) where

import Control.Lens

data Stalled = Stalled
  { _byFetch         :: Bool
  , _byDecode        :: Bool
  , _byReOrderBuffer :: Bool
  , _byIssue         :: Bool
  , _byExecute       :: Bool
  , _byWriteback     :: Bool
  }
  deriving (Show, Eq, Read)

-- Let Template Haskell make the lenses for StalledBy.
makeLenses ''Stalled

newStalled :: Stalled
newStalled = Stalled
  { _byFetch         = False
  , _byDecode        = False
  , _byReOrderBuffer = False
  , _byIssue         = False
  , _byExecute       = False
  , _byWriteback     = False
  }
