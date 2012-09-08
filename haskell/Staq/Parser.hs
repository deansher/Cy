{- ***** BEGIN LICENSE BLOCK *****
*
* Copyright (C) 2012 by Dean Thompson.  All Rights Reserved.
*
* The contents of this file are subject to the Apache 2 license:
* http://www.apache.org/licenses/LICENSE-2.0.html
*
* ***** END LICENSE BLOCK ***** -}

{-# LANGUAGE FlexibleInstances #-}

module Staq.Parser (
  parseTopLevelDecls
) where

import Staq.SyntaxTree (TopLevelDecl(..), Identifier(..))

import Data.List (intercalate, intersperse, isPrefixOf, isSuffixOf)

import Control.Applicative ( (<$>) )
import Control.Monad.Identity
import Control.Monad (liftM)

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Token (GenTokenParser)
import Text.Parsec.IndentParsec(runGIPT, foldedLinesOf)

import Text.Parsec.Token (makeTokenParser, GenLanguageDef(..))
import qualified Text.Parsec.IndentParsec.Token as IT
import Text.Parsec.IndentParsec.Prim

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
kwExport = IT.reserved tokP "export"
kwType = IT.reserved tokP "type"
kwAbstract = IT.reserved tokP "abstract"
identifier = IT.identifier tokP
opDot = IT.reservedOp tokP "."
opEqual = IT.reservedOp tokP "="
integer = IT.integer tokP
semiSepOrFoldedLines = IT.semiSepOrFoldedLines tokP
bracesBlock = IT.bracesBlock tokP
whiteSpace = IT.whiteSpace tokP

parseTopLevelDecls :: String -> String -> Either ParseError [TopLevelDecl]
parseTopLevelDecls sourceCode originName = parseStaq sourceCode originName topLevelDecls

parseStaq :: String -> String -> ParserM a -> Either ParseError a
parseStaq sourceCode originName production = runIdentity $ runGIPT production () originName sourceCode

type ParserM a = IndentParsecT String () Identity a

topLevelDecls :: ParserM [TopLevelDecl]
topLevelDecls = do whiteSpace
                   decls <- semiSepOrFoldedLines topLevelDecl
                   eof
                   return decls

topLevelDecl :: ParserM TopLevelDecl
topLevelDecl = packageDecl <|> export <|> typeDecl

packageDecl :: ParserM TopLevelDecl
packageDecl = do
  kwPackage
  name <- packageName
  return $ PackageDecl name

packageName :: ParserM String
packageName = liftM (intercalate ".") $ identifier `sepBy` opDot

export :: ParserM TopLevelDecl
export = do
  kwExport
  idents <- many1 $ identifier
  return $ Export $ map Identifier idents

typeDecl :: ParserM TopLevelDecl
typeDecl = do
  kwType
  typeName <- identifier
  opEqual
  typeExpr <- typeExpression
  return $ TypeDecl typeName typeExpr

typeExpression :: ParserM TypeExpression
typeExpression = componentLiteral <|> objectLiteral <?> "type expression"

componentLiteral :: ParserM TypeExpression
componentLiteral = do
  isAbstract <- indicator kwAbstract

indicator :: ParserM a -> ParserM Bool
indicator p = do
  m <- optionalMaybe p
  return $ case m of
            Just _ -> True
            Nothing -> False

----------------------
-- Test infrastructure

class RandomFormattable a where
  -- Given an indentation level and a RandomFormattable, produce a Gen String.
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
    white <- randomHorizontalWhitespace
    le <- randomLineEnding
    return $ indentation ++ "package" ++ white ++ name ++ le

  randomFormat indent (Export idents) = do
    let indentation = replicate indent ' '
    white <- randomHorizontalWhitespace
    le <- randomLineEnding
    foldedIdents <- randomFold indent idents
    return $ indentation ++ "export" ++ white ++ foldedIdents ++ le

instance RandomFormattable Identifier where
  randomFormat indent (Identifier name) = return name

randomFold :: (RandomFormattable a) => Int -> [a] -> Gen String
randomFold indent xs = concat <$> mapM (randomFold1 indent) xs

randomFold1 :: (RandomFormattable a) => Int -> a -> Gen String
randomFold1 indent x = do
  n <- choose(1, 4) :: Gen Int
  leadingWhite <- if n > 1 then randomHorizontalWhitespace
                           else do
                             le <- randomLineEnding
                             plusIndent <- choose(1, 4) :: Gen Int
                             let indentation = replicate (indent + plusIndent) ' '
                             return $ le ++ indentation
  formatX <- randomFormat indent x
  return $ leadingWhite ++ formatX

randomHorizontalWhitespace :: Gen String
randomHorizontalWhitespace = do
  n <- choose(1, 10) :: Gen Int
  if n > 1
    then
      randomSpaces 1 4
    else do
      s1 <- randomSpaces 0 2
      s2 <- randomSpaces 0 2
      c <- randomNestedComment
      return $ s1 ++ (deleteMatching "\n" c) ++ s2

randomBlankLines :: Int -> Gen String
randomBlankLines indent = do
  n <- choose(1, 4) :: Gen Int
  lines <- vectorOf n randomLineEnding :: Gen [String]
  return $ concat lines

randomLineEnding :: Gen String
randomLineEnding = do
  spaces <- randomSpaces 0 4
  n <- choose(1, 10) :: Gen Int
  if n <= 8
    then return $ spaces ++ "\n"
    else if n == 9
           then do
             text <- randomTextAvoiding ["\n"]
             return $ spaces ++ "--" ++ text ++ "\n"
           else do
             le <- randomLineEnding
             comment <- randomNestedComment
             return $ spaces ++ comment ++ le

randomNestedComment :: Gen String
randomNestedComment = do
  let curlyBuffer t = if "{" `isSuffixOf` t then " " else ""
  text <- randomTextAvoiding ["-}", "{-"]
  n <- choose(1, 10) :: Gen Int
  if n > 1
    then return $ "{-" ++ text ++ (curlyBuffer text) ++ "-}"
    else do
      text' <- randomTextAvoiding ["-}", "{-"]
      nested <- randomNestedComment
      return $ "{-" ++ text ++ nested ++ text' ++ (curlyBuffer text') ++ "-}"

randomTextAvoiding :: [String] -> Gen String
randomTextAvoiding substrings = do
  n <- choose(0, 10)
  raw <- vectorOf n arbitrary :: Gen String
  return $ eliminateAll substrings raw

eliminateAll :: Eq a => [[a]] -> [a] -> [a]
eliminateAll subseqs xs =
  firstFixedPoint (deleteMatchingAny subseqs) xs

eliminate :: Eq a => [a] -> [a] -> [a]
eliminate subseq xs =
  firstFixedPoint (deleteMatching subseq) xs

firstFixedPoint :: Eq a => (a -> a) -> a -> a
firstFixedPoint f x =
  let x' = f x in
    if x' == x then x else firstFixedPoint f x'

deleteMatchingAny :: Eq a => [[a]] -> [a] -> [a]
deleteMatchingAny subseqs xs =
  foldr deleteMatching xs subseqs

deleteMatching :: Eq a => [a] -> [a] -> [a]
deleteMatching subseq xs = del subseq xs []
  where del subseq [] ys = reverse ys
        del subseq xs ys | subseq `isPrefixOf` xs = del subseq (drop (length subseq) xs) ys
        del subseq (x:xs) ys = del subseq xs (x : ys)

randomSpaces :: Int -> Int -> Gen String
randomSpaces min max = do 
  n <- choose(min, max) :: Gen Int
  return $ replicate n ' '

prop_parseIsInverseOfRandomFormat :: [TopLevelDecl] -> Gen Property
prop_parseIsInverseOfRandomFormat decls = do
  indent <- choose(0, 4)
  formatted <- randomFormat indent decls 
  return $ case parseTopLevelDecls formatted "test input" of
             Left e       -> printTestCase (show e ++ "\n\n" ++ formatted) False
             Right decls' -> if decls' == decls
                               then property True
                               else printTestCase ("Original:  " ++ show decls ++ "\n\n" ++
                                                   "Parsed as: " ++ show decls' ++ "\n\n" ++
                                                   "Formatted as:" ++ "\n\n" ++ formatted) False

runTests = quickCheckWith (stdArgs { maxSuccess=1000, chatty=False }) prop_parseIsInverseOfRandomFormat