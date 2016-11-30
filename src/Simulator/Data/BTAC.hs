{-# LANGUAGE RankNTypes #-}
module Simulator.Data.BTAC
  ( module Simulator.Data.BTAC
  ) where

import Data.Word
import Data.Maybe
import Control.Lens

-- |This data type defines the states of saturation a branch can be in.
data Saturation = StronglyTaken
                | WeaklyTaken
                | WeaklyNotTaken
                | StronglyNotTaken
                deriving (Show, Eq, Read)

-- |This type defines a branch target address cache.
type BTAC = [ (Word32, (Maybe Word32, Saturation)) ]

-- |This defines an empty branch target address cache.
newBTAC :: BTAC
newBTAC = []

-- |This data type defines the control type of an instruction. If the
--  instruction isn't a branch instruction, then its control is NA. If the
--  instruction is a branch instruction then its control type is either taken
--  or not taken.
data Control = NA
             | Taken    Word32 Word32
             | NotTaken Word32
             deriving (Show, Eq, Read)

-- |This function returns true if the specified control is an NA.
isNA :: Control -> Bool
isNA       (NA          ) = True
isNA       _              = False

-- |This function returns true if the specified control is a Taken.
isTaken :: Control -> Bool
isTaken    (Taken    _ _) = True
isTaken    _              = False

-- |This function returns true if the specified control is a NotTaken.
isNotTaken :: Control -> Bool
isNotTaken (NotTaken _) = True
isNotTaken _            = False

-- |This function returns the target of the control.
getTarget :: Control -> Word32
getTarget (NA          ) = undefined
getTarget (NotTaken _  ) = undefined
getTarget (Taken    _ t) = t

-- |This function returns the pc of the control.
getPC :: Control -> Word32
getPC (NA           ) = undefined
getPC (NotTaken pc  ) = pc
getPC (Taken    pc _) = pc

-- |This lens provides a getter and setter for the branch target address And
--  saturation tags in the branch target address cache.
entry :: Word32 -> Lens' BTAC (Maybe Word32, Saturation)
entry pc = lens (\sc   -> lookupWithDefault (Nothing, StronglyTaken) pc sc)
                (\sc e -> update pc e sc                                  )

-- |This function returns true if the we should predict taken given the
--  specified saturation state.
predictTaken :: Saturation -> Bool
predictTaken s = s == StronglyTaken || s == WeaklyTaken

-- |This function returns the next saturation state given the current
--  saturation state and whether the branch was taken or not.
nextState :: Saturation -> Bool -> Saturation
nextState StronglyTaken    True  = StronglyTaken
nextState WeaklyTaken      True  = StronglyTaken
nextState WeaklyNotTaken   True  = WeaklyTaken
nextState StronglyNotTaken True  = WeaklyNotTaken
nextState StronglyTaken    False = WeaklyTaken
nextState WeaklyTaken      False = WeaklyNotTaken
nextState WeaklyNotTaken   False = StronglyNotTaken
nextState StronglyNotTaken False = StronglyNotTaken

-- |This function gets the value associated with the specified key @key@.
--  If @key@ is not in the association list, then @def@ is returned.
lookupWithDefault :: (Eq a) => b -> a -> [ (a, b) ] -> b
lookupWithDefault def key assocs = fromMaybe def . lookup key $ assocs

-- |This function updates the values associated with the specified key @key@.
--  If @key@ is not in the association list, the tuple (@key@, @val@) will be
--  appended to the end of the association list.
update :: (Eq a) => a -> b -> [ (a, b) ] -> [ (a, b) ]
update key val [] = [ (key, val) ]
update key val ((k, v):assocs)
  | k == key  = (key, val) : assocs
  | otherwise = (k, v) : update key val assocs
