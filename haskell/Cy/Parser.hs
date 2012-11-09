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

module Cy.Parser (
  parseModule
) where

import Cy.Language

import Data.List (intercalate, intersperse, isPrefixOf, isSuffixOf)

import Control.Applicative ( (<$>), (*>) )
import Control.Monad.Identity
import Control.Monad (liftM)

import qualified Data.Text as Text
import Data.Text (Text)

import Data.String (fromString)

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)

import Data.Foldable (toList)

import qualified Data.Strict.Maybe as SM

import Text.Parsec
import Text.Parsec.Combinator (eof)
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
                      , identStart = oneOf legalIdentifierFirstChars
                      , identLetter = oneOf legalIdentifierSubsequentChars
                      , opStart = oneOf legalOperatorFirstChars
                      , opLetter = oneOf legalOperatorSubsequentChars
                      
                      -- Check these for completeness periodically.
                      , reservedNames = reservedWords
                      , reservedOpNames = reservedOperators
                      
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
kwPackage = IT.reserved tokP "package"
kwPrivate = IT.reserved tokP "private"
kwPublic = IT.reserved tokP "public"
kwQualified = IT.reserved tokP "qualified"

-- When modifying these, change langDef too.
opAt = IT.reservedOp tokP "@"
opColon = IT.reservedOp tokP ":"
opComma = IT.reservedOp tokP ","
opDot = IT.reservedOp tokP "."
opEqual = IT.reservedOp tokP "="
opHyphen = IT.reservedOp tokP "-"
opPlus = IT.reservedOp tokP "+"
opSlash = IT.reservedOp tokP "/"

parseModule :: Text -> String -> Either ParseError ModuleDecl
parseModule sourceCode originName = parseCy sourceCode originName moduleDecl

parseCy :: Text -> String -> Parser a -> Either ParseError a
parseCy sourceCode originName production = runIdentity $ runGIPT production () originName sourceCode

type Parser a = IndentParsecT Text () Identity a

moduleDecl :: Parser ModuleDecl
moduleDecl = do
  whiteSpace
  (mid, props) <- (try topModuleHeader <|> subModuleHeader)
  exports <- possibleExportStatement
  imports <- importStatements
  decls <- topLevelDecls
  eof
  return $! ModuleDecl mid props exports imports decls

topModuleHeader :: Parser (ModuleId, ModuleProperties)
topModuleHeader = do
  kwPackage
  mid <- topModuleId
  version <- versionNumber
  return $! (mid, TopModuleProperties version)

subModuleHeader :: Parser (ModuleId, ModuleProperties)
subModuleHeader = do
  pub <- publicOrPrivate
  kwModule
  mid <- subModuleId
  return $! (mid, SubModuleProperties pub)

topModuleId :: Parser ModuleId
topModuleId = do
  (org, pkg) <- moduleOrgAndPackage
  return $! ModuleId org pkg TopModuleName

subModuleId :: Parser ModuleId
subModuleId = do
  (org, pkg) <- moduleOrgAndPackage
  opColon
  mod <- subModuleName
  return $! ModuleId org pkg mod

anyModuleId :: Parser ModuleId
anyModuleId = do
  (org, pkg) <- moduleOrgAndPackage
  mod <- (opColon >> subModuleName) <|> (return TopModuleName)
  return $! ModuleId org pkg mod

moduleOrgAndPackage :: Parser (OrgName, PackageName)
moduleOrgAndPackage = do
  org <- orgName
  opSlash
  pkg <- packageName
  return $! (org, pkg)

publicOrPrivate :: Parser Bool
publicOrPrivate = (try kwPublic >> return True) <|> (try kwPrivate >> return False)

orgName :: Parser OrgName
orgName = do
  name <- (try userAndDomain <|> domainName)
  return $! OrgName $ fromString name

userAndDomain :: Parser String
userAndDomain = do
  user <- userName
  sep <- (try opPlus >> return "+") <|> (try opAt >> return "@")
  domain <- domainName
  return $! user ++ sep ++ domain

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
  mid <- anyModuleId
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
  -- Given a base indentation level and a RandomFormattable, produce a Gen String.
  randomFormat :: Int -> a -> Gen String

instance RandomFormattable ModuleDecl where
  randomFormat indent decl = do

    let indentation = replicate indent ' '
        isTop = case midModuleName $ moduleId decl of
                  TopModuleName -> True
                  _             -> False

    header <- if isTop then randomFormatTopModuleHeader indent decl
                       else randomFormatSubModuleHeader indent decl

    exports <- randomFormatExports indent $ moduleExports decl
    imports <- randomFormatImports indent $ moduleImports decl
    decls <- randomFormatTopLevelDecls indent $ moduleDecls decl

    return $! indentation ++ header ++ exports ++ imports ++ decls

randomFormatTopModuleHeader :: Int -> ModuleDecl -> Gen String
randomFormatTopModuleHeader indent decl = do
  let indentation = replicate indent ' '
  white1 <- randomHorizontalWhitespace
  mid <- randomFormat indent $ moduleId decl
  white2 <- randomHorizontalWhitespace
  let version = formatVersionNumber $ modpropVersion $ moduleProperties decl
  le <- randomLineEnding
  return $! indentation ++ "package" ++ white1 ++ mid ++ white2 ++ version ++ le

randomFormatSubModuleHeader :: Int -> ModuleDecl -> Gen String
randomFormatSubModuleHeader indent decl = do
  let indentation = replicate indent ' '
      publicOrPrivate = if modpropPublic $ moduleProperties decl
                        then "public"
                        else "private"
  white1 <- randomHorizontalWhitespace
  white2 <- randomHorizontalWhitespace
  mid <- randomFormat indent $ moduleId decl
  le <- randomLineEnding
  return $! indentation ++ publicOrPrivate ++ white1 ++ "module" ++ white2 ++ mid ++ le

randomFormatExports :: Int -> Seq ModuleExport -> Gen String
randomFormatExports indent exports = do
  let indentation = replicate indent ' '
  white1 <- randomHorizontalWhitespace
  exportList <- randomFoldSepBy indent (map exportIdentifier $ toList exports) ","
  le <- randomLineEnding
  return $! indentation ++ "export" ++ white1 ++ "(" ++ exportList ++ ")" ++ le

randomFormatImports :: Int -> Seq ModuleImport -> Gen String
randomFormatImports indent imports = concat <$> mapM (randomFormatImport indent) (toList imports)

randomFormatImport :: Int -> ModuleImport -> Gen String
randomFormatImport indent imp = do
  let indentation = replicate indent ' '
  w <- forM [0..5] $ \i -> randomHorizontalWhitespace
  let wh n = w !! n
  let (qualifiedStr, asStr) = case importQualifier imp of
                                SM.Just ident -> ("qualified" ++ wh 0
                                                 , wh 1 ++ "as" ++ wh 2 ++ formatIdentifier ident)
                                SM.Nothing -> ("", "")
  midStr <- randomFormat indent (importModuleId imp)
  let idents = importIdentifiers imp
  identList <- if Seq.null idents
               then return ""
               else do
                 list <- randomFoldSepBy indent (toList idents) ","
                 return $! "(" ++ list ++ ")"
  le <- randomLineEnding
  return $! indentation ++ "import" ++ wh 3 ++ qualifiedStr ++ midStr ++ wh 4
            ++ asStr ++ wh 5 ++ identList ++ le

randomFormatTopLevelDecls :: Int -> Seq TopLevelDecl -> Gen String
randomFormatTopLevelDecls indent decls = return ""

instance RandomFormattable ModuleId where
  randomFormat indent mid = do
    let org = formatOrgName $ midOrgName mid
        pkg = formatPackageName $ midPackageName mid
        mod_suffix = case midModuleName mid of
                       TopModuleName -> ""
                       SubModuleName ident -> ":" ++ formatIdentifier ident
    return $! org ++ "/" ++ pkg ++ mod_suffix

instance RandomFormattable ModuleExport where
  randomFormat indent (ModuleExport ident) = randomFormat indent ident

instance RandomFormattable Identifier where
  randomFormat indent (Identifier name) = return $! Text.unpack name

-- | Given @indent@ and @xs@, randomly fold the @xs@ across lines while maintaining at least indent
-- level @indent@.  Each @x@ formatted after either some horizontal whitespace or a random line.
randomFold :: (RandomFormattable a) => Int -> [a] -> Gen String
randomFold indent xs = concat <$> mapM (randomFormatAndFold indent) xs

randomFormatAndFold :: (RandomFormattable a) => Int -> a -> Gen String
randomFormatAndFold indent x = do
  s <- randomFormat indent x
  randomFold1 indent s

-- | Given @indent@, @xs@, and @sep@, randomly fold the @xs@ across lines in the manner of @'randomFold'@, but
-- separated by @sep@.
randomFoldSepBy :: (RandomFormattable a) => Int -> [a] -> String -> Gen String
randomFoldSepBy indent xs sep = do
  fs <- forM xs $ \x -> randomFormat indent x
  let ss = intersperse sep fs
  concat <$> mapM (randomFold1 indent) ss

-- | Given @indent@ and @s@, generate a random fold of @s@ that can be appended to the current
-- code output.  @s@ will be preceded by either random horizontal whitespace or newlines plus
-- indentation back to at greater than @indent@.
randomFold1 :: Int -> String -> Gen String
randomFold1 indent s = do
  n <- choose(1, 4) :: Gen Int
  leadingWhite <- if n > 1 then randomHorizontalWhitespace
                           else do
                             le <- randomLineEnding
                             plusIndent <- choose(1, 4) :: Gen Int
                             let indentation = replicate (indent + plusIndent) ' '
                             return $! le ++ indentation
  return $! leadingWhite ++ s

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
             -- TODO: Be more sophisticated about when the space before is mandatory
             -- (to separate the comment start from an operator)
             return $! spaces ++ " --" ++ text ++ "\n"
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
                            else printTestCase ("Parsed as: " ++ show mod' ++ "\n\n" ++
                                                "Formatted as:" ++ "\n\n" ++ formatted) False

runTests = quickCheckWith (stdArgs { maxSuccess=1000 }) prop_correctlyParsesRandomlyFormattedCode

randomCode :: IO String
randomCode = do
   md <- head <$> sample' (arbitrary :: Gen ModuleDecl)
   head <$> sample' (randomFormat 0 md)