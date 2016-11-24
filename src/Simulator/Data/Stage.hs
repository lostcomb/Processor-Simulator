{-# LANGUAGE TemplateHaskell,
             MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleInstances,
             FlexibleContexts,
             RankNTypes #-}
module Simulator.Data.Stage
  ( module Simulator.Data.Stage
  , module Simulator.Data.Stage.Fetch
  , module Simulator.Data.Stage.Decode
  , module Simulator.Data.Stage.Issue
  , module Simulator.Data.Stage.Execute
  , module Simulator.Data.Stage.Writeback
  ) where

import Control.Lens

import Simulator.Data.Stall
import Simulator.Data.Stage.Fetch
import Simulator.Data.Stage.Decode
import Simulator.Data.Stage.Issue
import Simulator.Data.Stage.Execute
import Simulator.Data.Stage.Writeback

-- Let Template Haskell make the lenses for the stages.
makeFields ''Fetch
makeFields ''Decode
makeFields ''Issue
makeFields ''Execute
makeFields ''Writeback

isStalled :: (HasStalled a Stalled) => Lens' a Bool
isStalled = stalled.isStalled'
  where isStalled' :: Lens' Stalled Bool
        isStalled' = lens (\stall -> (_byFetch     stall) ||
                                     (_byDecode    stall) ||
                                     (_byIssue     stall) ||
                                     (_byExecute   stall) ||
                                     (_byWriteback stall))
                          (\stall _ -> stall)
