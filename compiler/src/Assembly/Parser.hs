module Assembly.Parser
  ( parseAssembly
  ) where

import Assembly.Instruction

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import Control.Applicative hiding (Const, (<|>), many)

parseAssembly :: String -> [ Instruction ]
parseAssembly str = case parse (whiteSpace lexer >> instructionParser) "" str of
                      Left  e -> error $ show e
                      Right r -> r

lexer = makeTokenParser languageDef
languageDef = emptyDef
  { commentLine   = ";"
  , reservedNames = [ "ADD", "SUB", "MUL", "DIV"
                    , "AND", "OR" , "XOR", "NOT"
                    , "JMP", "BEQ", "BGT", "BEZ"
                    , "LDC", "LDM", "STM", "NOP"
                    ]
  }

instructionParser :: Parser [ Instruction ]
instructionParser = many ((   labelParser
                          <|> aluParser
                          <|> notParser
                          <|> jumpParser
                          <|> binCondParser
                          <|> uniCondParser
                          <|> loadConstParser
                          <|> loadStoreParser
                          <|> nopParser
                          ) <* eof)

labelParser :: Parser Instruction
labelParser = LABEL <$ char ':' <*> identifier lexer

aluParser :: Parser Instruction
aluParser =   ADD <$ reserved lexer "ADD"
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
          <|> XOR <$ reserved lexer "XOR"
                  <*> registerParser <*> registerParser <*> registerParser

notParser :: Parser Instruction
notParser = NOT <$ reserved lexer "NOT"
                <*> registerParser
                <*> registerParser

jumpParser :: Parser Instruction
jumpParser = JMP <$ reserved lexer "JMP"
                 <*> registerParser

binCondParser :: Parser Instruction
binCondParser =   BEQ <$  reserved lexer "BEQ"
                      <*> offsetParser
                      <*> registerParser
                      <*> registerParser
              <|> BGT <$  reserved lexer "BGT"
                      <*> offsetParser
                      <*> registerParser
                      <*> registerParser

uniCondParser :: Parser Instruction
uniCondParser = BEZ <$ reserved lexer "BEZ"
                    <*> offsetParser
                    <*> registerParser

loadConstParser :: Parser Instruction
loadConstParser = LDC <$ reserved lexer "LDC"
                      <*> registerParser
                      <*> offsetParser

loadStoreParser :: Parser Instruction
loadStoreParser =   LDM <$  reserved lexer "LDM"
                        <*> registerParser
                        <*> registerParser
                <|> STM <$  reserved lexer "STM"
                        <*> registerParser
                        <*> registerParser

nopParser :: Parser Instruction
nopParser = reserved lexer "NOP" *> pure NOP

offsetParser :: Parser Offset
offsetParser =   Left  <$> constantParser
             <|> Right <$ char ':' <*> identifier lexer

registerParser :: Parser Register
registerParser = fromIntegral <$ (char 'R' <|> char 'r') <*> integer lexer

constantParser :: Parser Constant
constantParser = fromIntegral <$ char '#' <*> integer lexer
