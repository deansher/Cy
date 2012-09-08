{- ***** BEGIN LICENSE BLOCK *****
*
* Copyright (C) 2012 by Dean Thompson.  All Rights Reserved.
*
* The contents of this file are subject to the Apache 2 license:
* http://www.apache.org/licenses/LICENSE-2.0.html
*
* ***** END LICENSE BLOCK ***** -}

module Staq.SyntaxTree (
  TopLevelDecl(..), TypeDecl(..),
  Identifier(..), TypeExpression(..)
) where

import Control.Monad (replicateM)
import Data.List (intercalate)

import Test.QuickCheck

data TopLevelDecl = PackageDecl String
                  | Export [Identifier]
                  | TypeDecl Identifier TypeExpression
                  
  deriving (Eq, Show)

data Identifier = Identifier String
  deriving (Eq, Show)

data TypeExpression = ComponentLiteral {
                          typeIsAbstract :: Bool
                        , typeSuper :: Maybe[Identifier]
                        , componentElements :: [ComponentElement]
                      }
                    | ObjectLiteral {
                          typeIsAbstract :: Bool
                        , typeSuper :: Maybe[Identifier]
                        , objectElements :: [ObjectElement]
                      }

data ComponentElement = 

----------------------
-- Test infrastructure

instance Arbitrary TopLevelDecl where
  arbitrary = oneof [ arbitraryPackageDecl, arbitraryExport ]

arbitraryPackageDecl :: Gen TopLevelDecl
arbitraryPackageDecl = do
  len <- choose (1, 3) :: Gen Int
  names <- vectorOf len arbitraryIdentifierName
  return $ PackageDecl $ intercalate "." names

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
    first <- elements $ concat [letter, "_"]
    rest <- vectorOf (len - 1) $ elements $ concat [letter, digit, "_"]
    return $ Identifier $ first : rest

letter = concat [['a'..'z'], ['A'..'Z']]

digit = ['0'..'9']
  
