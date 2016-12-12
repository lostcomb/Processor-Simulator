module Simulator.Data.Association
  ( module Simulator.Data.Association
  ) where

import Data.Maybe (fromMaybe)

-- |This function gets the value associated with the specified key @key@.
--  If @key@ is not in the association list, then @def@ is returned.
searchWithDefault :: (Eq a) => b -> a -> [ (a, b) ] -> b
searchWithDefault def key = fromMaybe def . lookup key

-- |This function returns the value associated with the key @r@ applied to @f@.
--  Calls 'error' if the key @r@ is not in the association list.
searchWith :: (Eq a) => (b -> c) -> a -> [ (a, b) ] -> c
searchWith f r []           = error "searchWith: element not part of the specified list."
searchWith f r ((x, y) : l)
  | r == x                  = f y
  | otherwise               = searchWith f r l

-- |This function updates the values associated with the specified key @key@.
--  If @key@ is not in the association list, the tuple (@key@, @val@) will be
--  appended to the end of the association list.
update :: (Eq a) => a -> b -> [ (a, b) ] -> [ (a, b) ]
update key val [] = [ (key, val) ]
update key val ((k, v):assocs)
  | k == key  = (key, val) : assocs
  | otherwise = (k, v) : update key val assocs

-- |This function returns the new association list with the value associated
--  with the key @r@ applied to the function @f@. Calls 'error' if the key
--  @r@ is not in the association list.
updateWith :: (Eq a) => (b -> b) -> a -> [ (a, b) ] -> [ (a, b) ]
updateWith f r []           = error "updateWith: element not part of the specified list."
updateWith f r ((x, y) : l)
  | r == x                  = (x, f y) : l
  | otherwise               = (x,   y) : updateWith f r l
