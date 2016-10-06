module Assembly.Parser
  ( parseAssembly
  ) where

import Assembly.Instruction

import Control.Monad

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

parseAssembly :: String -> [ Instruction ]
parseAssembly str = case parse instructionParser "" str of
                      Left  e -> error $ show e
                      Right r -> r

lexer = Token.makeTokenParser languageDef
languageDef = emptyDef { Token.commentLine   = ";"
                       , Token.reservedNames = [ "ADD"
                                               , "SUB"
                                               , "MUL"
                                               , "DIV"
                                               , "AND"
                                               , "OR"
                                               , "XOR"
                                               , "NOT"
                                               , "JMP"
                                               , "BEQ"
                                               , "BGT"
                                               , "BEZ"
                                               , "LDC"
                                               , "LDM"
                                               , "STM"
                                               , "NOP"
                                               ]
                       }

instructionParser :: Parser [ Instruction ]
instructionParser = do insts <- many (   labelParser
                                     <|> aluParser
                                     <|> jumpParser
                                     <|> binCondParser
                                     <|> uniCondParser
                                     <|> loadConstParser
                                     <|> loadStoreParser
                                     <|> nopParser
                                     )
                       eof
                       return insts

labelParser :: Parser Instruction
labelParser = do char ':'
                 ident <- Token.identifier lexer
                 return $ LABEL ident

aluParser :: Parser Instruction
aluParser = do inst <- getInst
               rd   <- registerParser
               ri   <- registerParser
               rj   <- registerParser
               return $ inst rd ri rj
               where getInst = (   (Token.reserved lexer "ADD" >> return ADD)
                               <|> (Token.reserved lexer "SUB" >> return SUB)
                               <|> (Token.reserved lexer "MUL" >> return MUL)
                               <|> (Token.reserved lexer "DIV" >> return DIV)
                               <|> (Token.reserved lexer "AND" >> return AND)
                               <|> (Token.reserved lexer "OR"  >> return OR)
                               <|> (Token.reserved lexer "XOR" >> return XOR)
                               <|> (Token.reserved lexer "NOT" >> return NOT)
                               )

jumpParser :: Parser Instruction
jumpParser = do Token.reserved lexer "JMP"
                offset <- regOffsetParser
                return $ JMP offset

binCondParser :: Parser Instruction
binCondParser = do inst   <- getInst
                   offset <- offsetParser
                   ri     <- registerParser
                   rj     <- registerParser
                   return $ inst offset ri rj
                   where getInst = (   (Token.reserved lexer "BEQ" >> return BEQ)
                                   <|> (Token.reserved lexer "BGT" >> return BGT)
                                   )

uniCondParser :: Parser Instruction
uniCondParser = do Token.reserved lexer "BEZ"
                   offset <- offsetParser
                   ri     <- registerParser
                   return $ BEZ offset ri

loadConstParser :: Parser Instruction
loadConstParser = do Token.reserved lexer "LDC"
                     rd <- registerParser
                     c  <- constantParser
                     return $ LDC rd c

loadStoreParser :: Parser Instruction
loadStoreParser = do inst <- getInst
                     r1   <- registerParser
                     r2   <- registerParser
                     return $ inst r1 r2
                     where getInst = (   (Token.reserved lexer "LDM" >> return LDM)
                                     <|> (Token.reserved lexer "STM" >> return STM)
                                     )

nopParser :: Parser Instruction
nopParser = Token.reserved lexer "NOP" >> return NOP

regOffsetParser :: Parser RegOffset
regOffsetParser = (do r <- registerParser
                      return $ Left r
                  ) <|>
                  (do char ':'
                      l <- Token.identifier lexer
                      return $ Right l
                  )

offsetParser :: Parser Offset
offsetParser = (do c <- constantParser
                   return $ Left c
               ) <|>
               (do char ':'
                   l <- Token.identifier lexer
                   return $ Right l
               )

registerParser :: Parser Register
registerParser = do (char 'R' <|> char 'r')
                    reg <- Token.integer lexer
                    return $ fromIntegral reg

constantParser :: Parser Constant
constantParser = do char '#'
                    c <- Token.integer lexer
                    return $ fromIntegral c
