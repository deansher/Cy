{- ***** BEGIN LICENSE BLOCK *****
*
* Copyright (C) 2012 by Dean Thompson.  All Rights Reserved.
*
* The contents of this file are subject to the Apache 2 license:
* http://www.apache.org/licenses/LICENSE-2.0.html
*
* ***** END LICENSE BLOCK ***** -}

{-# LANGUAGE FlexibleInstances #-}

module Snappit.Parser (
  parseTopLevelDecls
) where

import Snappit.AbstractSyntaxTree (TopLevelDecl(..))

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Token (GenTokenParser)
import Text.Parsec.IndentParsec(runGIPT)

import Control.Monad.Identity
import Control.Monad (liftM)
import Text.Parsec.Token (makeTokenParser, GenLanguageDef(..))
import qualified Text.Parsec.IndentParsec.Token as IT
import Text.Parsec.IndentParsec.Prim

import Data.List (intercalate, intersperse)

import Test.QuickCheck

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
parseTopLevelDecls sourceCode originName = parseSnappit sourceCode originName topLevelDecls

parseSnappit :: String -> String -> ParserM a -> Either ParseError a
parseSnappit sourceCode originName production = runIdentity $ runGIPT production () originName sourceCode

type ParserM a = IndentParsecT String () Identity a

topLevelDecls :: ParserM [TopLevelDecl]
topLevelDecls = do whiteSpace
                   decls <- semiSep topLevelDecl
                   eof
                   return decls

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

----------------------
-- Test infrastructure

class RandomFormattable a where
  randomFormat :: Int -> a -> Gen String

instance RandomFormattable [TopLevelDecl] where
  randomFormat indent decls = do
    plusIndent <- choose (0, 4) :: Gen Int
    let declActions = map (randomFormat (indent + plusIndent)) decls
        actions = intersperse (randomBlankLines indent) declActions
    snippets <- sequence actions
    return $ concat snippets

instance RandomFormattable TopLevelDecl where
  randomFormat indent (PackageDecl name) = do
    let indentation = replicate indent ' '
    white <- randomSpaces 1 4
    return $ indentation ++ "package" ++ white ++ name ++ "\n"

randomBlankLines :: Int -> Gen String
randomBlankLines indent = do
  n <- choose(1, 4) :: Gen Int
  lines <- vectorOf n $ liftM (++ "\n") $ randomSpaces 0 (indent * 2) :: Gen [String]
  return $ concat lines

randomSpaces :: Int -> Int -> Gen String
randomSpaces min max = do 
  n <- choose(min, max) :: Gen Int
  return $ replicate n ' '

prop_parseComposeFormatIsId :: [TopLevelDecl] -> Gen Property
prop_parseComposeFormatIsId decls = do
  indent <- choose(0, 4)
  formatted <- randomFormat indent decls 
  return $ case parseTopLevelDecls formatted "test input" of
             Left e       -> printTestCase (show e ++ "\n\n" ++ formatted) False
             Right decls' -> if decls' == decls
                               then property True
                               else printTestCase ("Original:  " ++ show decls ++ "\n\n" ++
                                                   "Parsed as: " ++ show decls' ++ "\n\n" ++
                                                   "Formatted as:" ++ "\n\n" ++ formatted) False
