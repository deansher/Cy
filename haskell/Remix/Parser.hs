module Remix.Parser (
  parseTopLevelDecls
) where

import Remix.AbstractSyntaxTree (TopLevelDecl(..))

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Token (GenTokenParser)
import Text.Parsec.IndentParsec(runGIPT)

import Control.Monad.Identity
import Control.Monad (liftM)
import Text.Parsec.Token (makeTokenParser, GenLanguageDef(..))
import qualified Text.Parsec.IndentParsec.Token as IT
import Text.Parsec.IndentParsec.Prim

import Data.List (intercalate)

data Expr = Const Integer
          | Var String
          | WhereClause Expr [Binding] deriving Show

type Binding = (String, Expr)

type HaskellLikeIndent = IndentT HaskellLike Identity

langDef :: GenLanguageDef String () HaskellLikeIndent
langDef = LanguageDef { commentStart = "{-"
                      , commentEnd   = "-}"
                      , commentLine  = "--"
                      , identStart = letter   <|> char '_'
                      , identLetter = alphaNum <|> char '_'
                      , opStart = oneOf "-+/*=<>"
                      , opLetter = oneOf "-+/*=<>"
                      , reservedNames = [ "where" ]
                      , reservedOpNames = [ "=" , "+" , "-", "*", "/"]
                      , caseSensitive = False
                      , nestedComments = True
                      }

tokP :: IT.GenIndentTokenParser HaskellLike String () Identity
tokP = makeTokenParser langDef

kwPackage = IT.reserved tokP "package"
identifier = IT.identifier tokP
dot = IT.reservedOp tokP "."
integer = IT.integer tokP
semiSep = IT.semiSepOrFoldedLines tokP
whiteSpace = IT.whiteSpace tokP

parseTopLevelDecls :: String -> String -> Either ParseError [TopLevelDecl]
parseTopLevelDecls sourceCode originName = parseRemix sourceCode originName topLevelDecls

parseRemix :: String -> String -> ParserM a -> Either ParseError a
parseRemix sourceCode originName production = runIdentity $ runGIPT production () originName sourceCode

type ParserM a = IndentParsecT String () Identity a

topLevelDecls :: ParserM [TopLevelDecl]
topLevelDecls = do whiteSpace
                   semiSep topLevelDecl

topLevelDecl :: ParserM TopLevelDecl
topLevelDecl = packageDecl

packageDecl :: ParserM TopLevelDecl
packageDecl = do
  whiteSpace
  kwPackage
  name <- packageName
  return $ PackageDecl name

packageName :: ParserM String
packageName = liftM (intercalate ".") $ identifier `sepBy` dot

