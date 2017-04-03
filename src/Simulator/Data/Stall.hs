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
