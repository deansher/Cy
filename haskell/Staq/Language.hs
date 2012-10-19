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

module Staq.Language (
    ModuleDecl(..)
  , ModuleSpec(..)
  , ModuleId(..)
  , ModuleProperties(..)
  , OrgName(..)
  , PackageName(..)
  , ModuleName(..)
  , ModuleExport(..)
  , ModuleImport(..)
  , TopLevelDecl(..)
  , Identifier(..)
  , VersionNumber(..)
  , displayVersionNumber
) where

import Control.Monad (replicateM)

import qualified Data.Text as Text
import Data.Text (Text)

import Data.String (fromString)
import Data.List (intercalate)

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)

import Test.QuickCheck

data ModuleDecl = ModuleDecl {
    moduleSpec :: !ModuleSpec
  , moduleDecls :: !(Seq TopLevelDecl)
} deriving (Eq, Show, Read)

data ModuleSpec = ModuleSpec {
    moduleId :: !ModuleId
  , moduleProperties :: !ModuleProperties
  , moduleExports :: !(Seq ModuleExport)
  , moduleImports :: !(Seq ModuleImport)
} deriving (Eq, Show, Read)

data ModuleId = ModuleId {
    midOrgName :: !OrgName
  , midPackageName :: !PackageName
  , midModuleName :: !ModuleName
} deriving (Eq, Show, Read)

data ModuleProperties = TopModuleProperties {
    modpropVersion :: !VersionNumber
} | SubModuleProperties {
    modpropPublic :: !Bool
} deriving (Eq, Show, Read)

data OrgName = OrgName !Text
  deriving (Eq, Show, Read)

data PackageName = PackageName !(Seq Identifier)
  deriving (Eq, Show, Read)

data ModuleName = TopModuleName | SubModuleName !Identifier
  deriving (Eq, Show, Read)

data TopLevelDecl = TopLevelDecl
  deriving (Eq, Read, Show)

data Identifier = Identifier !Text
  deriving (Eq, Read, Show)

data ModuleExport = ModuleExport {
    modexportIdentifier :: !Identifier
} deriving (Eq, Read, Show)

data ModuleImport = ModuleImport {
    modimportModuleId :: !ModuleId
  , modimportIdentifiers :: !(Seq Identifier)
} deriving (Eq, Read, Show)

data VersionNumber = VersionNumber !Int !Int !Int !Text
  deriving (Eq, Read, Show)

displayVersionNumber (VersionNumber x y z build) =
  show x ++ "." ++ show y ++ "." ++ show z ++ "-" ++ Text.unpack build

----------------------
-- Test infrastructure

instance Arbitrary ModuleDecl where
  arbitrary = do
    mspec <- arbitrary :: Gen ModuleSpec
    len <- choose(1, 5) :: Gen Int
    decls <- vectorOf len arbitrary :: Gen [TopLevelDecl]
    return $! ModuleDecl mspec $ Seq.fromList decls

instance Arbitrary ModuleSpec where
  arbitrary = do
    mid <- arbitrary :: Gen ModuleId
    props <- arbitrary :: Gen ModuleProperties
    nexp <- choose(0, 3) :: Gen Int
    exports <- vectorOf nexp arbitrary :: Gen [ModuleExport]
    nimp <- choose(0, 3) :: Gen Int
    imports <- vectorOf nimp arbitrary :: Gen [ModuleImport]
    return $! ModuleSpec mid props (Seq.fromList exports) (Seq.fromList imports)

instance Arbitrary ModuleId where
  arbitrary = do
    org <- arbitrary :: Gen OrgName
    pkg <- arbitrary :: Gen PackageName
    name <- arbitrary :: Gen ModuleName
    return $! ModuleId org pkg name

instance Arbitrary ModuleProperties where
  arbitrary = do
    top <- arbitrary :: Gen Bool
    if top then do
      ver <- arbitrary :: Gen VersionNumber
      return $! TopModuleProperties ver
    else do
      pub <- arbitrary :: Gen Bool
      return $! SubModuleProperties pub

instance Arbitrary OrgName where
  arbitrary = do
    len <- choose (1, 3) :: Gen Int
    ds <- vectorOf len arbitraryIdentifierName
    let domain = intercalate "." ds
    hasUserName <- arbitrary :: Gen Bool
    if hasUserName then do
      u <- arbitraryIdentifierName
      return $! OrgName $ fromString $ domain ++ "+" ++ u
    else do
      return $! OrgName $ fromString $ domain

instance Arbitrary PackageName where
  arbitrary = do
    len <- choose (1, 3) :: Gen Int
    idents <- vectorOf len arbitrary :: Gen [Identifier]
    return $! PackageName $ Seq.fromList idents

instance Arbitrary ModuleName where
  arbitrary = do
    isTopLevel <- arbitrary :: Gen Bool
    if isTopLevel then do
      return $! TopModuleName
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
    return $! ModuleImport mid $ Seq.fromList idents

instance Arbitrary TopLevelDecl where
  arbitrary = return $! TopLevelDecl

arbitraryIdentifierName :: Gen String
arbitraryIdentifierName = do
  Identifier name <- arbitrary
  return $! Text.unpack name

instance Arbitrary Identifier where
  arbitrary = do 
    len <- choose (1, 8) :: Gen Int
    first <- elements $ concat [letters, "_"]
    rest <- vectorOf (len - 1) $ elements $ concat [letters, digits, "_"]
    return $! Identifier $ fromString $ first : rest

letters = concat [['a'..'z'], ['A'..'Z']]

digits = ['0'..'9']
  
instance Arbitrary VersionNumber where
  arbitrary = do
    x <- choose(0, 2) :: Gen Int
    y <- choose(0, 2) :: Gen Int
    z <- choose(0, 101) :: Gen Int
    Identifier build <- arbitrary :: Gen Identifier
    return $! VersionNumber x y z build
