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
module Simulator.Data.Setter
  ( module Simulator.Data.Setter
  ) where

import Control.Lens
import Control.Monad.State.Class as State

infix 4 ++?=
-- |This function applies the maybe append function to the target(s) of a
--  'Lens'', 'Iso', 'Setter' or 'Traversal'
(++?=) :: (MonadState s m) => ASetter' s (Maybe [ a ]) -> Maybe [ a ] -> m ()
l ++?= b = State.modify (l ++?~ b)

infix 4 ++=
  -- |This function applies the append function to the target(s) of a
  --  'Lens'', 'Iso', 'Setter' or 'Traversal'
(++=) :: (MonadState s m) => ASetter' s [ a ] -> [ a ] -> m ()
l ++= b = State.modify (l ++~ b)

infixr 4 ++?~
-- |This function applies the maybe append function to the target(s) of a
--  'Lens'', 'Iso', 'Setter' or 'Traversal'
(++?~) :: ASetter s t (Maybe [ a ]) (Maybe [ a ]) -> Maybe [ a ] -> s -> t
l ++?~ b = over l ((flip (++?)) b)

infixr 4 ++~
-- |This function applies the append function to the target(s) of a
--  'Lens'', 'Iso', 'Setter' or 'Traversal'
(++~) :: ASetter s t [ a ] [ a ] -> [ a ] -> s -> t
l ++~ b = over l ((flip (++)) b)

-- |This function appends the contents of two maybes which contain lists. It
--  propagates Nothing values.
(++?) :: Maybe [ a ] -> Maybe [ a ] -> Maybe [ a ]
(Just x) ++? (Just y) = Just (x ++ y)
_        ++? _        = Nothing
