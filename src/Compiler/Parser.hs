-- |This module defines a parser for the c-- language.

module Compiler.Parser
  ( Compiler.Parser.parse
  ) where

import Compiler.SyntaxTree

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Data.Functor.Identity
import Control.Applicative (pure, (<$), (<$>), (<*), (<*>), (*>))

lexer = makeTokenParser languageDef
languageDef =  emptyDef
  { commentStart    = "/*"
  , commentEnd      = "*/"
  , commentLine     = "//"
  , nestedComments  = True
  , reservedNames   = [ "int"    , "bool"   , "void"   , "true"
                      , "false"  , "if"     , "else"   , "while"
                      , "for"    , "return" , "main"   , "_return"
                      , "_end"   , "_return_addr"
                      ]
  , reservedOpNames = [ "=" , "+" , "-" , "*" , "/" , "=="
                      , "!=", "<" , ">" , "<=", ">=", "!"
                      , "&&", "||"
                      ]
  }

-- |This function returns the result of parsing @str@. Calls 'error' if @str@
--  is not valid c--.
parse :: String -> Program
parse str = case Text.Parsec.parse (whiteSpace lexer *> programParser <* eof) "" str of
              Left  e -> error $ show e
              Right r -> r

-- |A program is made up of a list of functions. There must be a function with
--  the identifier 'main' as the first function in the file.
programParser :: Parser Program
programParser = (:) <$> mainParser <*> many functionParser

-- |The syntax for the main function is:
--  <main> ::= 'void' 'main' '(' [ 'void' ] ')' '{' { <statement> } '}'
mainParser :: Parser Function
mainParser = Function <$> (reserved lexer "void" *> pure VOID)
                      <*> (reserved lexer "main" *> pure "main")
                      <*> parens lexer (option [] (reserved lexer "void" *> pure []))
                      <*> braces lexer (concat <$> many statementParser)

-- |The syntax for a function is:
--  <function> ::= <type> <identifier> '(' <arg> { ',' <arg> } ')' '{' { <statement> } '}'
functionParser :: Parser Function
functionParser = Function <$> funcTypeParser
                          <*> identifier lexer
                          <*> parens lexer (commaSep lexer argParser)
                          <*> braces lexer (concat <$> many statementParser)

-- |The syntax for a variable type is:
--  <type> ::= 'int'
--         |   'bool'
typeParser :: Parser Type
typeParser =   reserved lexer "int"  *> pure INT
           <|> reserved lexer "bool" *> pure BOOL

-- |The syntax for a function type is:
--  <func_type> ::= 'int'
--              |   'bool'
--              |   'void'
funcTypeParser :: Parser Type
funcTypeParser =   reserved lexer "int"  *> pure INT
               <|> reserved lexer "bool" *> pure BOOL
               <|> reserved lexer "void" *> pure VOID

-- |The syntax for an argument is:
--  <arg> ::= <type> <identifier> [ '[]' ]
argParser :: Parser Arg
argParser = Arg <$> typeParser
                <*> identifier lexer
                <*> option False (symbol lexer "[]" *> pure True)

-- |The syntax for a statement is:
--  <statement> ::= <type> <identifier> [ '[' <integer> ']' ] ';'
--              |   <identifier> [ '[' <expression> ']' ] '=' <expression> ';'
--              |   <type> <identifier> '=' <expression> ';'
--              |   'if' '(' <expression> ')' ('{' { <statement> } '}' | <statement>) [ 'else' ('{' { <statement> } '}' | <statement>) ]
--              |   'while' '(' <expression> ')' ('{' { <statement> } '}' | <statement>)
--              |   'for' '(' [ <assignDecl> ] ';' [ <expression> ] ';' [ <assignment> ] ')' ('{' { <statement> } '}' | <statement>)
--              |   <identifier> '(' [ <expression> { ',' <expression> } ] ')' ';'
--              |   'return' <expression> ';'
statementParser :: Parser [ Statement ]
statementParser =   toList <$> try (Declaration <$> typeParser
                                                <*> variableParser constantParser
                                                <*  semi lexer)
                <|> assignmentParser <* semi lexer
                <|> assignDeclParser <* semi lexer
                <|> toList <$> (Cond <$  reserved lexer "if"
                                     <*> parens lexer expressionParser
                                     <*> statementsParser
                                     <*> option []
                                         (reserved lexer "else"
                                      *> statementsParser))
                <|> toList <$> (While <$  reserved lexer "while"
                                      <*> parens lexer expressionParser
                                      <*> statementsParser)
                <|> do reserved lexer "for"
                       symbol lexer "("
                       decl  <- option [] assignDeclParser
                       semi lexer
                       expr  <- option TRUE expressionParser
                       semi lexer
                       ass   <- option [] assignmentParser
                       symbol lexer ")"
                       stmts <- statementsParser
                       return $ decl ++ [ While expr (stmts ++ ass)
                                        ]
                <|> toList <$> (FunctionCall <$> funcCallParser
                                             <*  semi lexer)
                <|> toList <$> (Return <$  reserved lexer "return"
                                       <*> expressionParser
                                       <*  semi lexer)
  where toList  a   = [ a    ]
        toList2 a b = [ a, b ]
        statementsParser = concat <$> (   braces lexer (many statementParser)
                                      <|> count 1 statementParser
                                      )
        assignmentParser = toList <$> try (Assignment <$> variableParser expressionParser
                                                      <*  reservedOp lexer "="
                                                      <*> expressionParser)
        assignDeclParser = do t    <- typeParser
                              var  <- variableParser constantParser
                              reservedOp lexer "="
                              expr <- expressionParser
                              return [ Declaration t var
                                     , Assignment  var expr
                                     ]

-- |The syntax for a variable is:
--  <variable> ::= <identifier>
--             |   <identifier> '[' <expression> ']'
variableParser :: Parser Expression -> Parser Variable
variableParser p =   try (Arr <$> identifier lexer <*> brackets lexer p)
                 <|> try (Var <$> identifier lexer                     )

-- |The syntax for a constant is:
--  <constant> ::= <integer>
constantParser :: Parser Expression
constantParser = Const . fromIntegral <$> integer lexer

-- |The syntax for a function call is:
--  <funcCall> ::= <identifier> '(' [ <expression> { ',' <expression> } ] ')'
funcCallParser :: Parser FuncCall
funcCallParser = FuncCall <$> identifier lexer
                          <*> parens lexer (commaSep lexer exprParser)

-- |The precedence of the operators (in descending order) is:
--  /, *, +, -, ==, !=, <, >, <=, >=, !, &&, ||
--  An expression can be either a function call, or an arithmetic / boolean
--  expression where all operands are not function calls.
expressionParser :: Parser Expression
expressionParser =   try (Func <$> funcCallParser)
                 <|> exprParser

exprParser :: Parser Expression
exprParser = buildExpressionParser expressionOps expressionTerm

expressionOps :: OperatorTable String u Data.Functor.Identity.Identity Expression
expressionOps = [ [ Infix  (reservedOp lexer "/"  >> return Div) AssocLeft ]
                , [ Infix  (reservedOp lexer "*"  >> return Mul) AssocLeft ]
                , [ Infix  (reservedOp lexer "+"  >> return Add) AssocLeft ,
                    Infix  (reservedOp lexer "-"  >> return Sub) AssocLeft ]
                , [ Infix  (reservedOp lexer "==" >> return Eq ) AssocLeft ,
                    Infix  (reservedOp lexer "!=" >> return Neq) AssocLeft ,
                    Infix  (reservedOp lexer "<"  >> return Lt ) AssocLeft ,
                    Infix  (reservedOp lexer ">"  >> return Gt ) AssocLeft ,
                    Infix  (reservedOp lexer "<=" >> return Lte) AssocLeft ,
                    Infix  (reservedOp lexer ">=" >> return Gte) AssocLeft ]
                , [ Prefix (reservedOp lexer "!"  >> return Neg)           ]
                , [ Infix  (reservedOp lexer "&&" >> return And) AssocLeft ,
                    Infix  (reservedOp lexer "||" >> return Or ) AssocLeft ]
                ]

expressionTerm :: Parser Expression
expressionTerm =   reserved lexer "true"  *> pure TRUE
               <|> reserved lexer "false" *> pure FALSE
               <|> Const . fromIntegral <$> integer lexer
               <|> try (EVar <$> variableParser exprParser)
               <|> parens lexer exprParser
