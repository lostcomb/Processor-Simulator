module Compiler.Parser
  ( parseStr
  , parseFile
  ) where

import Compiler.SyntaxTree

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import System.IO
import Control.Applicative hiding (Const, (<|>), many)
import Data.Functor.Identity

lexer = makeTokenParser languageDef

languageDef =  emptyDef
  { commentStart    = "/*"
  , commentEnd      = "*/"
  , commentLine     = "//"
  , nestedComments  = True
  , reservedNames   = [ "Int"   , "Bool"  , "True"  , "False"
                      , "if"    , "else"  , "while" , "for"
                      , "return"
                      ]
  , reservedOpNames = [ "=" , "+" , "-" , "*" , "/" , "=="
                      , "<" , ">" , "<=", ">=", "!" , "&&"
                      , "||"
                      ]
  }

parseStr :: String -> [ Function ]
parseStr str = case parse (whiteSpace lexer >> programParser) "" str of
                 Left  e -> error $ show e
                 Right r -> r

parseFile :: FilePath -> IO [ Function ]
parseFile path = do source <- readFile path
                    return $ parseStr source

programParser :: Parser [ Function ]
programParser = many1 functionParser

functionParser :: Parser Function
functionParser = Function <$> typeParser
                          <*> identifier lexer
                          <*> parens lexer (commaSep lexer argParser)
                          <*> braces lexer (many statementParser)

typeParser :: Parser Type
typeParser =   (reserved lexer "Int"  *> pure INT )
           <|> (reserved lexer "Bool" *> pure BOOL)

argParser :: Parser Arg
argParser = Arg <$> typeParser <*> identifier lexer

statementParser :: Parser Statement
statementParser =   try (Declaration  <$> decVarParser     <* semi lexer)
                <|> try (Assignment   <$> assignParser     <* semi lexer)
                <|> try (AssignDeclr  <$> assignDeclParser <* semi lexer)
                <|> (Cond         <$  reserved lexer "if"
                                  <*> parens lexer expressionParser
                                  <*> (braces lexer (many statementParser) <|> count 1 statementParser)
                                  <*> option []
                                      (reserved lexer "else"
                                   *> (braces lexer (many statementParser) <|> count 1 statementParser)))
                <|> (While        <$  reserved lexer "while"
                                  <*> parens lexer expressionParser
                                  <*> (braces lexer (many statementParser) <|> count 1 statementParser))
                <|> (For          <$  reserved lexer "for"
                                  <*  symbol lexer "("
                                  <*> optionMaybe assignDeclParser
                                  <*  semi lexer
                                  <*> optionMaybe expressionParser
                                  <*  semi lexer
                                  <*> optionMaybe assignParser
                                  <*  symbol lexer ")"
                                  <*> (braces lexer (many statementParser) <|> count 1 statementParser))
                <|> (FunctionCall <$> funcCallParser       <* semi lexer)
                <|> (Return       <$  reserved lexer "return"
                                  <*> expressionParser     <* semi lexer)

decVarParser :: Parser DecVar
decVarParser = DecVar <$> typeParser
                      <*> identifier lexer
                      <*> optionMaybe
                            (brackets lexer (Const . fromIntegral <$> integer lexer))

assVarParser :: Parser AssVar
assVarParser = AssVar <$> identifier lexer
                      <*> optionMaybe (brackets lexer (expressionParser))

assignParser :: Parser Assign
assignParser = Assign <$> assVarParser
                      <*  reservedOp lexer "="
                      <*> expressionParser

assignDeclParser :: Parser AssignDecl
assignDeclParser = AssignDecl <$> (DecVar <$> typeParser
                                          <*> identifier lexer
                                          <*> pure Nothing)
                              <*  reservedOp lexer "="
                              <*> expressionParser

funcCallParser :: Parser FuncCall
funcCallParser = FuncCall <$> identifier lexer
                          <*> parens lexer (commaSep lexer expressionParser)

expressionParser :: Parser Expression
expressionParser = buildExpressionParser expressionOps expressionTerm

expressionOps :: OperatorTable String u Data.Functor.Identity.Identity Expression
expressionOps = [ [ Infix  (reservedOp lexer "/"  >> return Div) AssocLeft ]
                , [ Infix  (reservedOp lexer "*"  >> return Mul) AssocLeft ]
                , [ Infix  (reservedOp lexer "+"  >> return Add) AssocLeft ,
                    Infix  (reservedOp lexer "-"  >> return Sub) AssocLeft ]
                , [ Infix  (reservedOp lexer "==" >> return Eq ) AssocLeft ,
                    Infix  (reservedOp lexer "<"  >> return Lt ) AssocLeft ,
                    Infix  (reservedOp lexer ">"  >> return Gt ) AssocLeft ,
                    Infix  (reservedOp lexer "<=" >> return Lte) AssocLeft ,
                    Infix  (reservedOp lexer ">=" >> return Gte) AssocLeft ]
                , [ Prefix (reservedOp lexer "!"  >> return Neg)           ]
                , [ Infix  (reservedOp lexer "&&" >> return And) AssocLeft ,
                    Infix  (reservedOp lexer "||" >> return Or ) AssocLeft ]
                ]

expressionTerm :: Parser Expression
expressionTerm =   reserved lexer "True"  *> pure TRUE
               <|> reserved lexer "False" *> pure FALSE
               <|> Const . fromIntegral <$> integer lexer
               <|> try (Func <$> funcCallParser)
               <|> try (Var  <$> assVarParser  )
               <|> parens lexer expressionParser
