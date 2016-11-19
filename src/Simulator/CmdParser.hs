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
             | Quit
             deriving (Show, Eq, Read)

lexer = makeTokenParser languageDef
languageDef = emptyDef
  { reservedNames = [ "step"      , "continue"
                    , "registers" , "memory"
                    , "stats"     , "decodei"
                    , "issuei"    , "executei"
                    , "writebacki", "quit"
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
--  <cmd> ::= 'step' [0-9]+
--        |   'continue'
--        |   'registers'
--        |   'memory'
--        |   'stats'
--        |   'decodei'
--        |   'issuei'
--        |   'executei'
--        |   'writebacki'
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
              <|> (Quit       <$  reserved lexer "quit"                    )
