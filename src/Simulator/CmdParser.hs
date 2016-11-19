-- |This module defines a parser for the commands to be used by the simulator
--  command line.

module Simulator.CmdParser
  ( Simulator.CmdParser.parse
  , Command(..)
  ) where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

data Command = Step Int
             | Continue
             | Registers
             | Memory
             | Stats
             | DecodeI
             | IssueI
             | ExecuteI
             | WritebackI
             | Set String Int
             | Get String
             | Quit
             deriving (Show, Eq, Read)

lexer = makeTokenParser languageDef
languageDef = emptyDef
  { reservedNames = [ "step"      , "continue"
                    , "registers" , "memory"
                    , "stats"     , "decodei"
                    , "issuei"    , "executei"
                    , "writebacki", "set"
                    , "get"       , "quit"
                    ]
  }

-- |This function returns the result of parsing @str@.
parse :: String -> Either ParseError [ Command ]
parse str = Text.Parsec.parse (whiteSpace lexer *> commandsParser <* eof) "" str

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
--        |   'decodei'
--        |   'issuei'
--        |   'executei'
--        |   'writebacki'
--        |   'set' <instruction_id> <integer>
--        |   'get' <instruction_id>
--        |   'quit'
commandParser :: Parser Command
commandParser =   (Step       <$  reserved lexer "step"
                              <*> option 1 (fromIntegral <$> integer lexer))
              <|> (Continue   <$  reserved lexer "continue"                )
              <|> (Registers  <$  reserved lexer "registers"               )
              <|> (Memory     <$  reserved lexer "memory"                  )
              <|> (Stats      <$  reserved lexer "stats"                   )
              <|> (DecodeI    <$  reserved lexer "decodei"                 )
              <|> (IssueI     <$  reserved lexer "issuei"                  )
              <|> (ExecuteI   <$  reserved lexer "executei"                )
              <|> (WritebackI <$  reserved lexer "writebacki"              )
              <|> (Set        <$  reserved lexer "set"
                              <*> identifier lexer
                              <*> (fromIntegral <$> integer lexer)         )
              <|> (Get        <$  reserved lexer "get"
                              <*> identifier lexer                         )
              <|> (Quit       <$  reserved lexer "quit"                    )
