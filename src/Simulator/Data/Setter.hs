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
