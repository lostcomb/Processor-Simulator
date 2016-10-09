module Compiler.Parser
  ( parseStr
  , parseFile
  ) where

import Compiler.SyntaxTree

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

import System.IO
import Control.Monad
import Data.Functor.Identity

lexer = Token.makeTokenParser languageDef

languageDef =  emptyDef { Token.commentStart    = "/*"
                        , Token.commentEnd      = "*/"
                        , Token.commentLine     = "//"
                        , Token.nestedComments  = True
                        , Token.reservedNames   = [ "Int"
                                                  , "Bool"
                                                  , "Void"
                                                  , "if"
                                                  , "else"
                                                  , "while"
                                                  , "for"
                                                  , "True"
                                                  , "False"
                                                  , "return"
                                                  ]
                        , Token.reservedOpNames = [ "="
                                                  , "+"
                                                  , "-"
                                                  , "*"
                                                  , "/"
                                                  , "=="
                                                  , "<"
                                                  , ">"
                                                  , "<="
                                                  , ">="
                                                  , "!"
                                                  , "&&"
                                                  , "||"
                                                  ]
                        }

parseStr :: String -> [ Function ]
parseStr str = case parse (Token.whiteSpace lexer >> programParser) "" str of
                 Left  e -> error $ show e
                 Right r -> r

parseFile :: FilePath -> IO [ Function ]
parseFile path = do source <- readFile path
                    return $ parseStr source

programParser :: Parser [ Function ]
programParser = many1 functionParser

functionParser :: Parser Function
functionParser = do cons  <- typeParser
                    ident <- Token.identifier lexer
                    args  <- Token.parens lexer $ argParser `sepBy` (Token.symbol lexer ",")
                    stms  <- Token.braces lexer statementsParser
                    return $ cons ident args stms
  where typeParser = (   (Token.reserved lexer "Void" >> return FunctionVoid)
                     <|> (Token.reserved lexer "Bool" >> return FunctionBool)
                     <|> (Token.reserved lexer "Int"  >> return FunctionInt )
                     )
        argParser  = (   (do Token.reserved lexer "Int"
                             ident <- Token.identifier lexer
                             return $ ArgInt ident
                         )
                     <|> (do Token.reserved lexer "Bool"
                             ident <- Token.identifier lexer
                             return $ ArgBool ident
                         )
                     )

statementsParser :: Parser [ Statement ]
statementsParser = many1 statementParser

statementParser :: Parser Statement
statementParser =   try assignDeclrParser
                <|> declarationParser
                <|> assignmentParser
                <|> condParser
                <|> whileParser
                <|> forParser
                <|> functionCallParser
                <|> returnParser

assignDeclrParser :: Parser Statement
assignDeclrParser = do assignDecl <- assignDeclParser
                       Token.symbol lexer ";"
                       return $ AssignDeclr assignDecl

declarationParser :: Parser Statement
declarationParser = do declare <- declareParser
                       Token.symbol lexer ";"
                       return $ Declaration declare

assignmentParser :: Parser Statement
assignmentParser = do assign <- assignParser
                      Token.symbol lexer ";"
                      return $ Assignment assign

condParser :: Parser Statement
condParser = do Token.reserved lexer "if"
                bexp <- Token.parens lexer bexpParser
                (try (do thenStms <- Token.braces lexer $ statementsParser
                         Token.reserved lexer "else"
                         elseStms <- Token.braces lexer $ statementsParser
                         return $ Cond bexp thenStms elseStms) <|>
                 try (do thenStms <- Token.braces lexer $ statementsParser
                         return $ Cond bexp thenStms []) <|>
                 try (do thenStm  <- statementParser
                         return $ Cond bexp [thenStm] []))

whileParser :: Parser Statement
whileParser = do Token.reserved lexer "while"
                 bexp <- Token.parens lexer bexpParser
                 (try (do stms <- Token.braces lexer $ statementsParser
                          return $ While bexp stms) <|>
                  try (do stm  <- statementParser
                          return $ While bexp [stm]))

forParser :: Parser Statement
forParser = do Token.reserved lexer "for"
               Token.symbol lexer "("
               decl   <- optionMaybe assignDeclParser
               Token.symbol lexer ";"
               bexp   <- optionMaybe bexpParser
               Token.symbol lexer ";"
               assign <- optionMaybe assignParser
               Token.symbol lexer ")"
               (try (do stms <- Token.braces lexer $ statementsParser
                        return $ For decl bexp assign stms) <|>
                try (do stm  <- statementParser
                        return $ For decl bexp assign [stm]))

functionCallParser :: Parser Statement
functionCallParser = do funcCall <- funcCallParser
                        Token.symbol lexer ";"
                        return $ FunctionCall funcCall

returnParser :: Parser Statement
returnParser = do Token.reserved lexer "return"
                  ret <- (liftM ReturnBexp bexpParser <|> liftM ReturnAexp aexpParser)
                  Token.symbol lexer ";"
                  return ret

assignDeclParser :: Parser AssignDecl
assignDeclParser =   (do Token.reserved lexer "Int"
                         ident <- Token.identifier lexer
                         index <- indexParser
                         Token.reservedOp lexer "="
                         aexp  <- aexpParser
                         return $ AssignDeclInt ident index aexp
                     )
                 <|> (do Token.reserved lexer "Bool"
                         ident <- Token.identifier lexer
                         index <- indexParser
                         Token.reservedOp lexer "="
                         bexp  <- bexpParser
                         return $ AssignDeclBool ident index bexp
                     )


indexParser :: Parser Int
indexParser = (   (liftM fromIntegral $ Token.brackets lexer (Token.integer lexer))
              <|> (return 0                                                       )
              )

declareParser :: Parser Declare
declareParser = do sType <- typeParser
                   ident <- Token.identifier lexer
                   index <- indexParser
                   return $ sType ident index
  where typeParser  = (   (Token.reserved lexer "Int"  >> return DeclareInt )
                      <|> (Token.reserved lexer "Bool" >> return DeclareBool)
                      )

assignParser :: Parser Assign
assignParser = do ident <- Token.identifier lexer
                  index <- indexParser
                  Token.reservedOp lexer "="
                  ((bexpParser >>= (\bexp -> return $ AssignBexp ident index bexp)) <|>
                   (aexpParser >>= (\aexp -> return $ AssignAexp ident index aexp)))

funcCallParser :: Parser FuncCall
funcCallParser = do ident  <- Token.identifier lexer
                    params <- Token.parens lexer $ paramParser `sepBy` (Token.symbol lexer ",")
                    return $ FuncCall ident params
  where paramParser = (   (liftM Right $ bexpParser)
                      <|> (liftM Left  $ aexpParser)
                      )

aexpParser :: Parser Aexp
aexpParser = buildExpressionParser aexpOps aexpTerm

aexpOps :: OperatorTable String u Data.Functor.Identity.Identity Aexp
aexpOps = [ [ Infix (Token.reservedOp lexer "/" >> return Div) AssocLeft ]
          , [ Infix (Token.reservedOp lexer "*" >> return Mul) AssocLeft ]
          , [ Infix (Token.reservedOp lexer "+" >> return Add) AssocLeft ,
              Infix (Token.reservedOp lexer "-" >> return Sub) AssocLeft ]
          ]

aexpTerm :: Parser Aexp
aexpTerm =   liftM (Const . fromIntegral) (Token.integer lexer)
         <|> (do ident <- Token.identifier lexer
                 index <- indexParser
                 return $ Var ident index
             )
         <|> Token.parens lexer aexpParser

bexpParser :: Parser Bexp
bexpParser = buildExpressionParser bexpOps bexpTerm

bexpOps :: OperatorTable String u Data.Functor.Identity.Identity Bexp
bexpOps = [ [ Prefix (Token.reservedOp lexer "!"  >> return Neg)           ]
          , [ Infix  (Token.reservedOp lexer "&&" >> return And) AssocLeft ,
              Infix  (Token.reservedOp lexer "||" >> return Or ) AssocLeft ]
          ]

bexpTerm :: Parser Bexp
bexpTerm =   (Token.reserved lexer "True"  >> return TRUE )
         <|> (Token.reserved lexer "False" >> return FALSE)
         <|> try relationParser
         <|> Token.parens lexer bexpParser

relationParser :: Parser Bexp
relationParser = do a1   <- aexpParser
                    cons <- relation
                    a2   <- aexpParser
                    return $ cons a1 a2
  where relation = (   (Token.reservedOp lexer "==" >> return Eq )
                   <|> (Token.reservedOp lexer "<"  >> return Lt )
                   <|> (Token.reservedOp lexer ">"  >> return Gt )
                   <|> (Token.reservedOp lexer "<=" >> return Lte)
                   <|> (Token.reservedOp lexer ">=" >> return Gte)
                   )
