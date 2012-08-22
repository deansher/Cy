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

data TopLevelDecl = PackageDecl String
  deriving (Show)