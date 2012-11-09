{- ***** BEGIN LICENSE BLOCK *****
*
* Copyright (C) 2012 by Dean Thompson.  All Rights Reserved.
*
* The contents of this file are subject to the Apache 2 license:
* http://www.apache.org/licenses/LICENSE-2.0.html
*
* ***** END LICENSE BLOCK ***** -}

{-# LANGUAGE BangPatterns, OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Cy.Language (
    reservedWords
  , reservedOperators
  , ModuleDecl(..)
  , ModuleId(..)
  , ModuleProperties(..)
  , OrgName(..)
  , legalDomainLabelFirstChars
  , legalDomainLabelSubsequentChars
  , legalUserNameFirstChars
  , legalUserNameSubsequentChars
  , legalIdentifierFirstChars
  , legalIdentifierSubsequentChars
  , legalOperatorFirstChars
  , legalOperatorSubsequentChars
  , formatOrgName
  , PackageName(..)
  , formatPackageName
  , ModuleName(..)
  , ModuleExport(..)
  , ModuleImport(..)
  , TopLevelDecl(..)
  , Identifier(..)
  , formatIdentifier
  , VersionNumber(..)
  , formatVersionNumber
) where

import Control.Monad (replicateM)

import qualified Data.Text as Text
import Data.Text (Text)

import qualified Data.Strict.Maybe as SM

import Data.String (fromString)
import Data.List (intercalate)

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)

import Data.Foldable (toList)

import Test.QuickCheck

reservedWords = [ "as", "export", "import", "module", "package"
                ,"private", "public", "qualified"
                ]

reservedOperators = [ "@", ";", ",", ".", "=", "-", "+", "/" ]

data ModuleDecl = ModuleDecl {
    moduleId :: !ModuleId
  , moduleProperties :: !ModuleProperties
  , moduleExports :: !(Seq ModuleExport)
  , moduleImports :: !(Seq ModuleImport)
  , moduleDecls :: !(Seq TopLevelDecl)
} deriving (Eq, Show, Read)

data ModuleId = ModuleId {
    midOrgName :: !OrgName
  , midPackageName :: !PackageName
  , midModuleName :: !ModuleName
} deriving (Eq, Show, Read)

midTopLevel mid = moduleNameTopLevel $ midModuleName mid

data ModuleProperties = TopModuleProperties {
    modpropVersion :: !VersionNumber
} | SubModuleProperties {
    modpropPublic :: !Bool
} deriving (Eq, Show, Read)

data OrgName = OrgName !Text
  deriving (Eq, Show, Read)

formatOrgName (OrgName name) = Text.unpack name

data PackageName = PackageName !(Seq Identifier)
  deriving (Eq, Show, Read)

formatPackageName (PackageName idents) =
  intercalate "/" $ map formatIdentifier $ toList idents

data ModuleName = TopModuleName | SubModuleName !Identifier
  deriving (Eq, Show, Read)

moduleNameTopLevel TopModuleName = True
moduleNameTopLevel (SubModuleName _) = False

data TopLevelDecl = TopLevelDecl
  deriving (Eq, Read, Show)

data Identifier = Identifier !Text
  deriving (Eq, Read, Show)

formatIdentifier (Identifier name) = Text.unpack name

data ModuleExport = ModuleExport {
    exportIdentifier :: !Identifier
} deriving (Eq, Read, Show)

-- todo: Add support for "hiding"
data ModuleImport = ModuleImport {
    importModuleId :: !ModuleId
  , importQualifier :: !(SM.Maybe Identifier)
  , importIdentifiers :: !(Seq Identifier)
} deriving (Eq, Read, Show)

data VersionNumber = VersionNumber !Int !Int !Int !Text
  deriving (Eq, Read, Show)

formatVersionNumber (VersionNumber x y z build) =
  show x ++ "." ++ show y ++ "." ++ show z ++ "-" ++ Text.unpack build

----------------------
-- Test infrastructure

instance Arbitrary ModuleDecl where
  arbitrary = do
    mid <- arbitrary :: Gen ModuleId
    props <- genModuleProperties $ midTopLevel mid
    nexp <- choose(0, 3) :: Gen Int
    exports <- vectorOf nexp arbitrary :: Gen [ModuleExport]
    nimp <- choose(0, 3) :: Gen Int
    imports <- vectorOf nimp arbitrary :: Gen [ModuleImport]
    -- len <- choose(1, 5) :: Gen Int
    -- decls <- vectorOf len arbitrary :: Gen [TopLevelDecl]
    let decls = []
    return $! ModuleDecl mid props (Seq.fromList exports) (Seq.fromList imports) (Seq.fromList decls)

instance Arbitrary ModuleId where
  arbitrary = do
    org <- arbitrary :: Gen OrgName
    pkg <- arbitrary :: Gen PackageName
    name <- arbitrary :: Gen ModuleName
    return $! ModuleId org pkg name

genModuleProperties :: Bool -> Gen ModuleProperties
genModuleProperties top = do
    if top then do
      ver <- arbitrary :: Gen VersionNumber
      return $! TopModuleProperties ver
    else do
      pub <- arbitrary :: Gen Bool
      return $! SubModuleProperties pub

legalDomainLabelFirstChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
legalDomainLabelSubsequentChars = '-' : legalDomainLabelFirstChars

legalUserNameFirstChars = '_' : legalDomainLabelFirstChars
legalUserNameSubsequentChars = '.' : legalUserNameFirstChars

instance Arbitrary OrgName where
  arbitrary = do
    len <- choose (1, 3) :: Gen Int
    ds <- vectorOf len arbitraryDomainLabel
    let domain = intercalate "." ds
    hasUserName <- arbitrary :: Gen Bool
    if hasUserName then do
      u <- arbitraryUserName
      sep <- elements ["+", "@"]
      return $! OrgName $ fromString $ u ++ sep ++ domain
    else do
      return $! OrgName $ fromString $ domain

arbitraryDomainLabel = genIdent 8 legalDomainLabelFirstChars legalDomainLabelSubsequentChars

arbitraryUserName = genIdent 8 legalUserNameFirstChars legalUserNameSubsequentChars

instance Arbitrary PackageName where
  arbitrary = do
    len <- choose (1, 3) :: Gen Int
    idents <- vectorOf len arbitrary :: Gen [Identifier]
    return $! PackageName $ Seq.fromList idents

instance Arbitrary ModuleName where
  arbitrary = do
    isTopLevel <- arbitrary :: Gen Bool
    if isTopLevel then do
      return TopModuleName
    else do
      n <- arbitrary :: Gen Identifier
      return $! SubModuleName n

instance Arbitrary ModuleExport where
  arbitrary = do
    ident <- arbitrary :: Gen Identifier
    return $! ModuleExport ident

instance Arbitrary ModuleImport where
  arbitrary = do
    mid <- arbitrary :: Gen ModuleId
    len <- choose (1, 3) :: Gen Int
    idents <- vectorOf len arbitrary :: Gen [Identifier]
    qualifier <- oneof [return SM.Nothing, SM.Just `fmap` (arbitrary :: Gen Identifier)]
    return $! ModuleImport mid qualifier $ Seq.fromList idents

instance Arbitrary TopLevelDecl where
  arbitrary = return TopLevelDecl

instance Arbitrary Identifier where
  arbitrary = do 
    name <- arbitraryIdentifierName
    return $! Identifier $ fromString $ name

legalIdentifierFirstChars = concat [letters, "_"]
legalIdentifierSubsequentChars = concat [letters, digits, "_"]

arbitraryIdentifierName :: Gen String
arbitraryIdentifierName = do
  name <- genIdent 8 legalIdentifierFirstChars legalIdentifierSubsequentChars
  if name `elem` reservedWords
  then arbitraryIdentifierName
  else return $! name

genIdent :: Int -> [Char] -> [Char] -> Gen [Char]
genIdent maxLen legalFirsts legalRests = do
  len <- choose (1, maxLen) :: Gen Int
  first <- elements legalFirsts
  rest <- vectorOf (len - 1) $ elements legalRests
  return $! first : rest

letters = concat [['a'..'z'], ['A'..'Z']]

digits = ['0'..'9']
  
legalOperatorFirstChars = ".,-+/*=<>;"
legalOperatorSubsequentChars = legalOperatorFirstChars

instance Arbitrary VersionNumber where
  arbitrary = do
    x <- choose(0, 2) :: Gen Int
    y <- choose(0, 2) :: Gen Int
    z <- choose(0, 101) :: Gen Int
    Identifier build <- arbitrary :: Gen Identifier
    return $! VersionNumber x y z build
