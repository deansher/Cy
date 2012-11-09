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

module Cy.Compiler (
  main
) where

import System.Directory (getCurrentDirectory)
import Control.Monad (forM_, filterM)
import Control.Applicative ((<$>))

main :: IO ()
main = do
  currentHome <- findCurrentHomeDir
  homeDirs <- findHomeDirs currentHome
  currentHomeModules <- findModules currentHome
  dirtyCurrentHomeModules <- filterM moduleIsDirty currentHomeModules 
  forM_ dirtyCurrentHomeModules $ \dirtyModule -> do
    rebuild dirtyModule

data Module = Module

findCurrentHomeDir :: IO FilePath
findCurrentHomeDir = getCurrentDirectory

findHomeDirs :: FilePath -> IO [FilePath]
findHomeDirs currentHome = return [currentHome]

findModules :: FilePath -> IO [Module]
findModules homeDir = return []

moduleIsDirty :: Module -> IO Bool
moduleIsDirty m = return False

rebuild :: Module -> IO ()
rebuild m = return ()
