-- |This module defines a parser for the assembly language used by this
--  processor.

module Assembler.Parser
  ( Assembler.Parser.parse
  ) where

import Assembler.Instruction

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Control.Applicative (pure, (<$), (<$>), (<*), (<*>), (*>))

lexer = makeTokenParser languageDef
languageDef = emptyDef
  { commentLine   = ";"
  , reservedNames = [ "ADD", "SUB", "MUL", "DIV"
                    , "AND", "OR" , "NOT", "JMP"
                    , "BEZ", "CEQ", "CGT", "LDC"
                    , "LDM", "STM", "NOP", "HALT"
                    ]
  }

-- |This function returns the result of parsing @str@. Calls 'error' if @str@
--  is not valid assembly.
parse :: String -> [ Instruction ]
parse str = case Text.Parsec.parse (whiteSpace lexer *> instructionParser <* eof) "" str of
              Left  e -> error $ show e
              Right r -> r

-- |The syntax for an instruction is:
--  <instruction> ::= <label>
--                |   <aluComp>
--                |   <not>
--                |   <jump>
--                |   <uniCond>
--                |   <loadConst>
--                |   <loadStore>
--                |   <misc>
instructionParser :: Parser [ Instruction ]
instructionParser = many (   labelParser
                         <|> aluCompParser
                         <|> notParser
                         <|> jumpParser
                         <|> uniCondParser
                         <|> loadConstParser
                         <|> loadStoreParser
                         <|> miscParser
                         )

-- |The syntax for a label is:
--  <label> ::= ':' <identifier>
labelParser :: Parser Instruction
labelParser = LABEL <$ char ':' <*> identifier lexer

-- |The syntax for an arithmetic and logic instsruction is:
--  <aluComp> ::= 'ADD' <register> <register> <register>
--        |   'SUB' <register> <register> <register>
--        |   'MUL' <register> <register> <register>
--        |   'DIV' <register> <register> <register>
--        |   'AND' <register> <register> <register>
--        |   'OR'  <register> <register> <register>
--        |   'CEQ' <Register> <Register> <Register>
--        |   'CGT' <Register> <Register> <Register>
aluCompParser :: Parser Instruction
aluCompParser =   ADD <$ reserved lexer "ADD"
                      <*> registerParser <*> registerParser <*> registerParser
              <|> SUB <$ reserved lexer "SUB"
                      <*> registerParser <*> registerParser <*> registerParser
              <|> MUL <$ reserved lexer "MUL"
                      <*> registerParser <*> registerParser <*> registerParser
              <|> DIV <$ reserved lexer "DIV"
                      <*> registerParser <*> registerParser <*> registerParser
              <|> AND <$ reserved lexer "AND"
                      <*> registerParser <*> registerParser <*> registerParser
              <|> OR  <$ reserved lexer "OR"
                      <*> registerParser <*> registerParser <*> registerParser
              <|> CEQ <$ reserved lexer "CEQ"
                      <*> registerParser <*> registerParser <*> registerParser
              <|> CGT <$ reserved lexer "CGT"
                      <*> registerParser <*> registerParser <*> registerParser

-- |The syntax for a NOT instruction is:
--  <not> ::= 'NOT' <register> <register>
notParser :: Parser Instruction
notParser = NOT <$ reserved lexer "NOT"
                <*> registerParser
                <*> registerParser

-- |The syntax for a JMP instruction is:
--  <jump> ::= 'JMP' <register>
jumpParser :: Parser Instruction
jumpParser = JMP <$ reserved lexer "JMP"
                 <*> registerParser

-- |The syntax for a unary conditional branch instruction is:
--  <uniCond> ::= 'BEZ' <offset> <register>
uniCondParser :: Parser Instruction
uniCondParser = BEZ <$ reserved lexer "BEZ"
                    <*> registerParser
                    <*> offsetParser

-- |The syntax for a load constant instruction is:
--  <loadConst> ::= 'LDC' <register> <offset>
loadConstParser :: Parser Instruction
loadConstParser = LDC <$ reserved lexer "LDC"
                      <*> registerParser
                      <*> offsetParser

-- |The syntax for a load / store instruction is:
--  <loadStore> ::= 'LDM' <register> <register>
--              |   'STM' <register> <register>
loadStoreParser :: Parser Instruction
loadStoreParser =   LDM <$  reserved lexer "LDM"
                        <*> registerParser
                        <*> registerParser
                <|> STM <$  reserved lexer "STM"
                        <*> registerParser
                        <*> registerParser

-- |The syntax for a no operation instruction is:
--  <misc> ::= 'NOP'
--         |   'HALT'
miscParser :: Parser Instruction
miscParser =   reserved lexer "NOP"  *> pure NOP
           <|> reserved lexer "HALT" *> pure HALT

-- |The syntax for an offset is:
--  <offset> ::= <constant>
--           |   ':' <identifier>
offsetParser :: Parser Offset
offsetParser =   Left  <$> constantParser
             <|> Right <$ char ':' <*> identifier lexer

-- |The syntax for a register is:
--  <register> ::= ('r' | 'R') <integer>
registerParser :: Parser Register
registerParser = fromIntegral <$ (char 'R' <|> char 'r') <*> integer lexer

-- |The syntax for a constant is:
--  <constant> ::= '#' <integer>
constantParser :: Parser Constant
constantParser = fromIntegral <$ char '#' <*> integer lexer
