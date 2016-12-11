{-# LANGUAGE RankNTypes #-}
module Simulator.Data.ReservationStation
  ( module Simulator.Data.ReservationStation
  ) where

import Data.Int
import Control.Lens

import Simulator.Data.Registers
import Simulator.Data.Instruction
import Simulator.Data.Association

-- |This type defines a reservation station entry.
type ReservationStationEntry = (Int, InstructionReg, Maybe Int, Maybe Int, Int32, Int32)

-- |This type defines a reservation station. It is simply a list of instructions
--  from which to select an instruction to execute.
type ReservationStation = ([ ReservationStationEntry ], Bool, Int)

-- |This defines an empty reservation station.
newReservationStation :: Int -> ReservationStation
newReservationStation n = ([], False, n)

-- |This function updates the reservation station entries with the specified
--  values.
updatePointerValues :: (Int, Int32) -> ReservationStation -> ReservationStation
updatePointerValues (p, v) (is, busy, n) = (is', busy, n)
  where is' = map (\i -> updateVi . updateVj $ i) is
        updateVi (instId, inst, Just qi, qj, vi, vj)
          = if qi == p then (instId, inst, Nothing, qj,  v, vj)
                       else (instId, inst, Just qi, qj, vi, vj)
        updateVj (instId, inst, qi, Just qj, vi, vj)
          = if qj == p then (instId, inst, qi, Nothing, vi,  v)
                       else (instId, inst, qi, Just qj, vi, vj)

-- |This type defines a register alias table.
type RegisterAliasTable = [ (Register, Maybe Int) ]

-- |This defines an empty register result status buffer.
newRegisterAliasTable :: RegisterAliasTable
newRegisterAliasTable = zip [(minBound::Register)..] (repeat Nothing)

-- |This lens defines a getter and setter for the entries in the register alias
--  table.
status :: Register -> Lens' RegisterAliasTable (Maybe Int)
status r = lens (\rat    -> searchWithDefault Nothing r rat)
                (\rat rs -> update r rs rat                )
