-- |This module defines a parser for the c-- language.

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
import Data.Functor.Identity
import Control.Applicative hiding (Const, (<|>), many)

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

-- |This function returns the result of parsing @str@. Calls 'error' if @str@
--  is not valid c--.
parseStr :: String -> Program
parseStr str = case parse (whiteSpace lexer >> programParser) "" str of
                 Left  e -> error $ show e
                 Right r -> r

-- |This function returns the result of parsing the file specified by @path@.
--  Calls 'error' if the file specified by @path@ doesn't contain valid c--.
parseFile :: FilePath -> IO Program
parseFile path = do source <- readFile path
                    return $ parseStr source

-- |A program is made up of a list of functions. There must be one function with
--  the identifier 'main', although this is not enforced at this level.
programParser :: Parser Program
programParser = many1 functionParser

-- |The syntax for a function is:
--  <function> ::= <type> <identifier> '(' <arg> { ',' <arg> } ')' '{' { <statement> } '}'
functionParser :: Parser Function
functionParser = Function <$> typeParser
                          <*> identifier lexer
                          <*> parens lexer (commaSep lexer argParser)
                          <*> braces lexer (many statementParser)

-- |The syntax for a type is:
--  <type> ::= 'Int'
--         |   'Bool'
typeParser :: Parser Type
typeParser =   (reserved lexer "Int"  *> pure INT )
           <|> (reserved lexer "Bool" *> pure BOOL)

-- |The syntax for an argument is:
--  <arg> ::= <type> <identifier>
argParser :: Parser Arg
argParser = Arg <$> typeParser <*> identifier lexer

-- |The syntax for a statement is:
--  <statement> ::= <type> <identifier> [ '[' <integer> ']' ] ';'
--              |   <identifier> [ '[' <expression> ']' ] '=' <expression> ';'
--              |   <type> <identifier> '=' <expression> ';'
--              |   'if' '(' <expression> ')' ('{' { <statement> } '}' | <statement>) [ 'else' ('{' { <statement> } '}' | <statement>) ]
--              |   'while' '(' <expression> ')' ('{' { <statement> } '}' | <statement>)
--              |   'for' '(' [ <assignDecl> ] ';' [ <expression> ] ';' [ <assignment> ] ')' ('{' { <statement> } '}' | <statement>)
--              |   <identifier> '(' <expression> { ',' <expression> } ')' ';'
--              |   <identifier> '(' ')' ';'
--              |   'return' <expression> ';'
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

-- |The syntax for a variable declaration is:
--  <decVar> ::= <type> <identifier> [ '[' <expression> ']' ]
decVarParser :: Parser DecVar
decVarParser = DecVar <$> typeParser
                      <*> identifier lexer
                      <*> optionMaybe
                            (brackets lexer (Const . fromIntegral <$> integer lexer))

-- |The syntax for a variable assignment is:
--  <assVar> ::= <identifier> [ '[' <expression> ']' ]
assVarParser :: Parser AssVar
assVarParser = AssVar <$> identifier lexer
                      <*> optionMaybe (brackets lexer (expressionParser))

-- |The syntax for an assignment is:
-- <assignment> ::= <assVar> '=' <expression>
assignParser :: Parser Assign
assignParser = Assign <$> assVarParser
                      <*  reservedOp lexer "="
                      <*> expressionParser

-- |The syntax for a declaration with assignment is:
--  <assignmentDecl> ::= <type> <identifier> '=' <expression>
assignDeclParser :: Parser AssignDecl
assignDeclParser = AssignDecl <$> (DecVar <$> typeParser
                                          <*> identifier lexer
                                          <*> pure Nothing)
                              <*  reservedOp lexer "="
                              <*> expressionParser

-- |The syntax for a function call is:
--  <funcCall> ::= <identifier> '(' <expression> { ',' <expression> } ')'
--             |   <identifier> '(' ')'
funcCallParser :: Parser FuncCall
funcCallParser = FuncCall <$> identifier lexer
                          <*> parens lexer (commaSep lexer expressionParser)

-- |The precedence of the operators (in descending order) is:
--  /, *, +, -, ==, <, >, <=, >=, !, &&, ||
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
