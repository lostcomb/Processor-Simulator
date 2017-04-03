{-
This file is part of aca-processor-simulator.

aca-processor-simulator is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

aca-processor-simulator is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with aca-processor-simulator.  If not, see <http://www.gnu.org/licenses/>.
-}
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
  , module Simulator.Data.Stage.ReOrderBuffer
  , module Simulator.Data.Stage.Issue
  , module Simulator.Data.Stage.Execute
  , module Simulator.Data.Stage.Writeback
  ) where

import Control.Lens

import Simulator.Data.Stall
import Simulator.Data.Stage.Fetch
import Simulator.Data.Stage.Decode
import Simulator.Data.Stage.ReOrderBuffer
import Simulator.Data.Stage.Issue
import Simulator.Data.Stage.Execute
import Simulator.Data.Stage.Writeback

-- Let Template Haskell make the lenses for the stages.
makeFields ''Fetch
makeFields ''Decode
makeFields ''ReOrderBuffer
makeFields ''Issue
makeFields ''Execute
makeFields ''Writeback

isStalled :: (HasStalled a Stalled) => Lens' a Bool
isStalled = stalled.isStalled'
  where isStalled' :: Lens' Stalled Bool
        isStalled' = lens (\stall -> (_byFetch         stall) ||
                                     (_byDecode        stall) ||
                                     (_byReOrderBuffer stall) ||
                                     (_byIssue         stall) ||
                                     (_byExecute       stall) ||
                                     (_byWriteback     stall))
                          (\stall _ -> stall)
