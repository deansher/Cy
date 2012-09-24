{- ***** BEGIN LICENSE BLOCK *****
*
* Copyright (C) 2012 by Dean Thompson.  All Rights Reserved.
*
* The contents of this file are subject to the Apache 2 license:
* http://www.apache.org/licenses/LICENSE-2.0.html
*
* ***** END LICENSE BLOCK ***** -}

module Staq.SyntaxTree (
    TopLevelDecl(..)
  , Identifier(..)
  , VersionNumber(..)
) where

import Control.Monad (replicateM)
import Data.List (intercalate)

import Test.QuickCheck

data TopLevelDecl = PackageDecl String VersionNumber
                  | Export [Identifier]
                  
  deriving (Eq, Show)

data Identifier = Identifier String
  deriving (Eq, Show)

data VersionNumber = VersionNumber Int Int Int String
  deriving (Eq)

instance Show VersionNumber where
  show (VersionNumber x y z build) = (show x) ++ "." ++ (show y) ++ "." ++ (show z) ++ buildSuffix
    where buildSuffix = if length build == 0 then "" else "-" ++ build

----------------------
-- Test infrastructure

instance Arbitrary TopLevelDecl where
  arbitrary = oneof [ arbitraryPackageDecl, arbitraryExport ]

arbitraryPackageDecl :: Gen TopLevelDecl
arbitraryPackageDecl = do
  len <- choose (1, 3) :: Gen Int
  names <- vectorOf len arbitraryIdentifierName
  v <- arbitrary :: Gen VersionNumber
  return $ PackageDecl (intercalate "." names) v

arbitraryExport = do
  n <- choose (1, 3) :: Gen Int
  idents <- vectorOf n arbitrary :: Gen [Identifier]
  return $ Export idents

arbitraryIdentifierName :: Gen String
arbitraryIdentifierName = do
  Identifier name <- arbitrary
  return name

instance Arbitrary Identifier where
  arbitrary = do 
    len <- choose (1, 8) :: Gen Int
    first <- elements $ concat [letters, "_"]
    rest <- vectorOf (len - 1) $ elements $ concat [letters, digits, "_"]
    return $ Identifier $ first : rest

letters = concat [['a'..'z'], ['A'..'Z']]

digits = ['0'..'9']
  
instance Arbitrary VersionNumber where
  arbitrary = do
    x <- choose(0, 2) :: Gen Int
    y <- choose(0, 2) :: Gen Int
    z <- choose(0, 101) :: Gen Int
    Identifier build <- arbitrary :: Gen Identifier
    return $ VersionNumber x y z build
