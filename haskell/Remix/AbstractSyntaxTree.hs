{- ***** BEGIN LICENSE BLOCK *****
*
* Copyright (C) 2012 by Dean Thompson.  All Rights Reserved.
*
* The contents of this file are subject to the Apache 2 license:
* http://www.apache.org/licenses/LICENSE-2.0.html
*
* ***** END LICENSE BLOCK ***** -}

module Remix.AbstractSyntaxTree (
  TopLevelDecl(..)
) where

import Control.Monad (replicateM)
import Data.List (intercalate)

import Test.QuickCheck

data TopLevelDecl = PackageDecl String
  deriving (Show)

data Identifier = Identifier String

----------------------
-- Test infrastructure

instance Arbitrary TopLevelDecl where
  arbitrary = oneof [ arbitraryPackageDecl ]

arbitraryPackageDecl :: Gen TopLevelDecl
arbitraryPackageDecl = do
  len <- choose (1, 3) :: Gen Int
  names <- vectorOf len arbitraryIdentifierName
  return $ PackageDecl $ intercalate "." names

arbitraryIdentifierName :: Gen String
arbitraryIdentifierName = do
  Identifier name <- arbitrary
  return name

instance Arbitrary Identifier where
  arbitrary = do 
    len <- choose (1, 8) :: Gen Int
    first <- elements $ concat [letter, "_"]
    rest <- vectorOf (len - 1) $ elements $ concat [letter, digit, "_"]
    return $ Identifier $ first : rest

letter = concat [['a'..'z'], ['A'..'Z']]

digit = ['0'..'9']
  
