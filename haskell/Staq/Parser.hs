{- ***** BEGIN LICENSE BLOCK *****
*
* Copyright (C) 2012 by Dean Thompson.  All Rights Reserved.
*
* The contents of this file are subject to the Apache 2 license:
* http://www.apache.org/licenses/LICENSE-2.0.html
*
* ***** END LICENSE BLOCK ***** -}

{-# LANGUAGE FlexibleInstances, BangPatterns, OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Staq.Parser (
  parseModule
) where

import Staq.Language hiding (moduleId)

import Data.List (intercalate, intersperse, isPrefixOf, isSuffixOf)

import Control.Applicative ( (<$>), (*>) )
import Control.Monad.Identity
import Control.Monad (liftM)

import qualified Data.Text as Text
import Data.Text (Text)

import Data.String (fromString)

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)

import qualified Data.Strict.Maybe as SM

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Token (makeTokenParser, GenTokenParser, GenLanguageDef(..))
import Text.Parsec.Text () -- We just need the Stream instance declaration for Text

import Text.Parsec.IndentParsec(runGIPT, foldedLinesOf)
import qualified Text.Parsec.IndentParsec.Token as IT
import Text.Parsec.IndentParsec.Prim
import Text.Parsec.IndentParsec.Combinator

import Test.QuickCheck

type HaskellLikeIndent = IndentT HaskellLike Identity

langDef :: GenLanguageDef Text () HaskellLikeIndent
langDef = LanguageDef { commentStart = "{-"
                      , commentEnd   = "-}"
                      , commentLine  = "--"
                      , identStart = letter <|> char '_'
                      , identLetter = alphaNum <|> char '_' <|> char '\''
                      , opStart = oneOf ".,-+/*=<>;"
                      , opLetter = oneOf ".,-+/*=<>;"
                      -- Check these for completeness periodically.
                      , reservedNames = [ "as", "export", "import", "module", "private", "public", "qualified" ]
                      , reservedOpNames = [ ";", ",", ".", "=", "-", "+", "/" ]
                      , caseSensitive = True
                      , nestedComments = True
                      }

tokP :: IT.GenIndentTokenParser HaskellLike Text () Identity
tokP = makeTokenParser langDef

bracesBlock = IT.bracesBlock tokP
parens = IT.parens tokP
rawIdentifier = IT.identifier tokP
integer = IT.integer tokP
whiteSpace = IT.whiteSpace tokP
semiSepOrFoldedLines = IT.semiSepOrFoldedLines tokP
commaSepOrFoldedLines = IT.commaSepOrFoldedLines tokP

-- When modifying these, change langDef too.
kwAs = IT.reserved tokP "as"
kwExport = IT.reserved tokP "export"
kwImport = IT.reserved tokP "import"
kwModule = IT.reserved tokP "module"
kwPrivate = IT.reserved tokP "private"
kwPublic = IT.reserved tokP "public"
kwQualified = IT.reserved tokP "qualified"

-- When modifying these, change langDef too.
opColon = IT.reservedOp tokP ":"
opComma = IT.reservedOp tokP ","
opDot = IT.reservedOp tokP "."
opEqual = IT.reservedOp tokP "="
opHyphen = IT.reservedOp tokP "-"
opPlus = IT.reservedOp tokP "+"
opSlash = IT.reservedOp tokP "/"

parseModule :: Text -> String -> Either ParseError ModuleDecl
parseModule sourceCode originName = parseStaq sourceCode originName moduleDecl

parseStaq :: Text -> String -> Parser a -> Either ParseError a
parseStaq sourceCode originName production = runIdentity $ runGIPT production () originName sourceCode

type Parser a = IndentParsecT Text () Identity a

moduleDecl :: Parser ModuleDecl
moduleDecl = do
  pub <- publicOrPrivate
  kwModule
  mid <- moduleId
  exports <- possibleExportStatement
  imports <- importStatements
  decls <- topLevelDecls
  
  let props = SubModuleProperties pub -- TODO: How does TopModuleProperties work?
     
  return $! ModuleDecl mid props exports imports decls

moduleId :: Parser ModuleId
moduleId = do
  org <- orgName
  opSlash
  pkg <- packageName
  mod <- (opColon >> subModuleName) <|> return TopModuleName
  return $! ModuleId org pkg mod

publicOrPrivate :: Parser Bool
publicOrPrivate = (kwPublic >> return True) <|> (kwPrivate >> return False)

orgName :: Parser OrgName
orgName = do
  name <- (try userPlusDomainName <|> domainName)
  return $! OrgName $ fromString name

userPlusDomainName :: Parser String
userPlusDomainName = do
  user <- userName
  opPlus
  domain <- domainName
  return $! user ++ "+" ++ domain

legalDomainLabelFirstChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
legalDomainLabelSubsequentChars = '-' : legalDomainLabelSubsequentChars

legalUserNameFirstChars = '.' : legalDomainLabelSubsequentChars
legalUserNameSubsequentChars = '-' : legalUserNameFirstChars

userName :: Parser String
userName = do
  first <- oneOf legalUserNameFirstChars
  rest <- many $ oneOf legalUserNameSubsequentChars
  return $! first : rest

domainName :: Parser String
domainName = do
  labels <- domainNameLabel `sepBy` opDot
  return $! intercalate "." labels

domainNameLabel :: Parser String
domainNameLabel = do
  first <- oneOf legalDomainLabelFirstChars
  rest <- many $ oneOf legalDomainLabelSubsequentChars
  return $! first : rest

packageName :: Parser PackageName
packageName = do
  idents <- identifier `sepBy` opSlash
  return $! PackageName $ Seq.fromList idents

identifier :: Parser Identifier
identifier = do
  r <- rawIdentifier
  return $! Identifier $ fromString r

subModuleName :: Parser ModuleName
subModuleName = do
  ident <- identifier
  return $! SubModuleName ident
  
versionNumber :: Parser VersionNumber
versionNumber = do
  x <- integer
  opDot
  y <- integer
  opDot
  z <- integer
  build <- (opHyphen *> rawIdentifier) <|> (return "")
  return $! VersionNumber (fromIntegral x) (fromIntegral y) (fromIntegral z) (fromString build)

possibleExportStatement :: Parser (Seq ModuleExport)
possibleExportStatement = exportStatement <|> (return $! Seq.empty)
                        
exportStatement :: Parser (Seq ModuleExport)
exportStatement = foldedLinesOf $ do
  kwExport
  idents <- parens $ identifier `sepBy` opComma
  return $! Seq.fromList $ map ModuleExport idents

importStatements :: Parser (Seq ModuleImport)
importStatements = Seq.fromList <$> many importStatement

importStatement :: Parser ModuleImport
importStatement = foldedLinesOf $ do
  kwImport
  isQualified <- option False (kwQualified >> return True)
  mid <- moduleId
  qual <- if isQualified
          then kwAs >> (SM.Just <$> identifier)
          else return SM.Nothing
  idents <- option Seq.empty importIdentifierList
  return $! ModuleImport mid qual idents

importIdentifierList :: Parser (Seq Identifier)
importIdentifierList = parens (Seq.fromList <$> identifier `sepBy` opComma)

topLevelDecls :: Parser (Seq TopLevelDecl)
topLevelDecls = return Seq.empty

----------------------
-- Test infrastructure

class RandomFormattable a where
  -- Given an indentation level and a RandomFormattable, produce a Gen String.
  randomFormat :: Int -> a -> Gen String

instance RandomFormattable ModuleDecl where
  randomFormat indent moduleDecl = return ""

instance RandomFormattable Identifier where
  randomFormat indent (Identifier name) = return $! Text.unpack name

instance RandomFormattable VersionNumber where
  randomFormat ident v =
    return $! displayVersionNumber v

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
                             return $! le ++ indentation
  formatX <- randomFormat indent x
  return $! leadingWhite ++ formatX

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
      return $! s1 ++ (deleteMatching "\n" c) ++ s2

randomBlankLines :: Int -> Gen String
randomBlankLines indent = do
  n <- choose(1, 4) :: Gen Int
  lines <- vectorOf n randomLineEnding :: Gen [String]
  return $! concat lines

randomLineEnding :: Gen String
randomLineEnding = do
  spaces <- randomSpaces 0 4
  n <- choose(1, 10) :: Gen Int
  if n <= 8
    then return $! spaces ++ "\n"
    else if n == 9
           then do
             text <- randomTextAvoiding ["\n"]
             return $! spaces ++ "--" ++ text ++ "\n"
           else do
             le <- randomLineEnding
             comment <- randomNestedComment
             return $! spaces ++ comment ++ le

randomNestedComment :: Gen String
randomNestedComment = do
  let curlyBuffer t = if "{" `isSuffixOf` t then " " else ""
  text <- randomTextAvoiding ["-}", "{-"]
  n <- choose(1, 10) :: Gen Int
  if n > 1
    then return $! "{-" ++ text ++ (curlyBuffer text) ++ "-}"
    else do
      text' <- randomTextAvoiding ["-}", "{-"]
      nested <- randomNestedComment
      return $! "{-" ++ text ++ nested ++ text' ++ (curlyBuffer text') ++ "-}"

randomTextAvoiding :: [String] -> Gen String
randomTextAvoiding substrings = do
  n <- choose(0, 10)
  raw <- vectorOf n arbitrary :: Gen String
  return $! eliminateAll substrings raw

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
  return $! replicate n ' '

prop_correctlyParsesRandomlyFormattedCode :: ModuleDecl -> Gen Property
prop_correctlyParsesRandomlyFormattedCode mod = do
  indent <- choose(0, 4)
  formatted <- randomFormat indent mod
  return $! case parseModule (Text.pack formatted) "test input" of
              Left e     -> printTestCase (show e ++ "\n\n" ++ formatted) False
              Right mod' -> if mod' == mod
                            then property True
                            else printTestCase ("Original:  " ++ show mod ++ "\n\n" ++
                                                "Parsed as: " ++ show mod' ++ "\n\n" ++
                                                "Formatted as:" ++ "\n\n" ++ formatted) False

runTests = quickCheckWith (stdArgs { maxSuccess=1000, chatty=False }) prop_correctlyParsesRandomlyFormattedCode