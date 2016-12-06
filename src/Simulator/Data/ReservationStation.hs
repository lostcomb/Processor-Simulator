module Simulator.Data.ReservationStation
  ( module Simulator.Data.ReservationStation
  ) where

import Data.Int

import Simulator.Data.Registers
import Simulator.Data.Instruction

-- |This type defines a reservation station. It is simply a list of instructions
--  from which to select an instruction to execute.
type ReservationStation = [ Instruction (Either Register Int32) Int ]

-- |This defines an empty reservation station.
newReservationStation :: ReservationStation
newReservationStation = []
