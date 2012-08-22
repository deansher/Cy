module Remix.AbstractSyntaxTree (
  TopLevelDecl(..)
) where

data TopLevelDecl = PackageDecl String
  deriving (Show)