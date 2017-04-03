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
-- |This module defines a parser for the commands to be used by the simulator
--  command line.
module Simulator.CommandLine.Parser
  ( Simulator.CommandLine.Parser.parse
  ) where

import Data.List
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Error
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Control.Applicative (pure, (<$), (<$>), (<*), (<*>), (*>))

import Simulator.CommandLine.Command

lexer = makeTokenParser languageDef
languageDef = emptyDef
  { reservedNames = [ "step"      , "continue"
                    , "registers" , "memory"
                    , "stats"     , "fetchi"
                    , "decodei"   , "reorderi"
                    , "issuei"    , "executei"
                    , "writebacki", "set"
                    , "get"       , "latches"
                    , "caches"    , "rob"
                    , "quit"      , "show w"
                    , "show c"
                    ]
  }

-- |This function returns the result of parsing @str@.
parse :: String -> Either String [ Command ]
parse str = case Text.Parsec.parse (whiteSpace lexer *> commandsParser <* eof) "" str of
  (Left  e) -> let messages = errorMessages e
                   message_strs = init . drop 2
                                       . filter (not . elem '\'')
                                       . nub
                                       . filter (not . null)
                                       . map messageString $ messages
                   message_list = intercalate ", " message_strs
               in Left $ "Invalid command. Expecting:\n" ++ message_list
  (Right r) -> Right r

-- |The syntax for a list of commands is:
--  <cmds> ::= <cmd> [ ';' <cmd> ]*
commandsParser :: Parser [ Command ]
commandsParser = commandParser `sepBy` semi lexer

-- |The syntax for a command is:
--  <cmd> ::= 'step' <integer>
--        |   'continue'
--        |   'registers'
--        |   'memory'
--        |   'stats'
--        |   'fetchi'
--        |   'decodei'
--        |   'reorderi'
--        |   'issuei'
--        |   'executei'
--        |   'writebacki'
--        |   'set' <instruction_id> <integer>
--        |   'get' <instruction_id>
--        |   'latches'
--        |   'caches'
--        |   'rob'
--        |   'quit'
--        |   'show w'
--        |   'show c'
commandParser :: Parser Command
commandParser =   (Step       <$  reserved lexer "step"
                              <*> option 1 (fromIntegral <$> integer lexer))
              <|> (Continue   <$  reserved lexer "continue"                )
              <|> (Registers  <$  reserved lexer "registers"               )
              <|> (Memory     <$  reserved lexer "memory"                  )
              <|> (Stats      <$  reserved lexer "stats"                   )
              <|> (FetchI     <$  reserved lexer "fetchi"                  )
              <|> (DecodeI    <$  reserved lexer "decodei"                 )
              <|> (ReOrderI   <$  reserved lexer "reorderi"                )
              <|> (IssueI     <$  reserved lexer "issuei"                  )
              <|> (ExecuteI   <$  reserved lexer "executei"                )
              <|> (WritebackI <$  reserved lexer "writebacki"              )
              <|> (Set        <$  reserved lexer "set"
                              <*> identifier lexer
                              <*> (fromIntegral <$> integer lexer)         )
              <|> (Get        <$  reserved lexer "get"
                              <*> identifier lexer                         )
              <|> (Latches    <$  reserved lexer "latches"                 )
              <|> (Caches     <$  reserved lexer "caches"                  )
              <|> (ROB        <$  reserved lexer "rob"                     )
              <|> (Quit       <$  reserved lexer "quit"                    )
              <|> (ShowW      <$  reserved lexer "show w"                  )
              <|> (ShowC      <$  reserved lexer "show c"                  )
