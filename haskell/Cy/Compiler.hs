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

-- For now, the Cy compiler is a command-line tool that loads all needed state, incrementally
-- rebuilds all modified source under a specified Cy root, and exits.  The compiler will become a
-- daemon that watches for filesystem changes and incrementally rebuilds as needed.

module Cy.Compiler (
  main
) where

import System.Directory (getCurrentDirectory)
import Control.Monad (mapM)
import Control.Applicative ((<$>))

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)

import qualified Data.Strict.Maybe as SM

-- A RootState represents the known state (partly definitional and partly cached) of a Cy root directory.
data RootState = RootState {
    rstateDir :: !FilePath 
  , rstatePackages :: !(Seq PackageState)
}

initRootState :: FilePath -> RootState
initRootState rootDir = RootState { rstateDir = rootDir, rstatePackages = SM.Nothing }

-- A PackageState represents the cached state of a Cy package.
data PackageState = PackageState {
    pstateName :: !PackageName
  , pstateModules :: !(Seq ModuleState)
}

data ModuleState = ModuleState {
    mstateName :: !ModuleName
}

-- Options are the compiler's command-line options and (potentially) environment variables.
data Options = Options {
    optionsErrorMessages :: !(Seq String)
  , optionsRootDir :: !FilePath
  , optionsOtherRootDirs :: FilePath
}

optionsHasErrors :: Options -> Bool
optionsHasErrors options = !Seq.empty $ optionsErrorMessages options

main :: IO ()
main = do
  options <- parseOptions
  if optionsHasErrors
  then printUsage
  else rootState <- makeRootState $ optionsRootDir options
       otherRootStates <- mapM makeRootState $ optionsOtherRootDirs options
       incrementallyRebuildRoot rootState otherRootStates options
       where makeRootState = updateRootStateFromFilesystem . initRootState

updateRootStateFromFilesystem :: RootState -> IO RootState

incrementallyRebuildRoot :: RootState -> [RootState] -> Options -> IO ()
incrementallyRebuildRoot rstate otherRStates options = do
  rebuiltPackages <- mapM incrementallyRebuildPackage $ rstatePackages rstate
  rstate { rstatePackages = rebuiltPackages }

 ---------------------------------------------------------------------
 -- Root exception type for all compiler exceptions

 data SomeCompilerException = forall e . Exception e => SomeCompilerException e
     deriving Typeable

 instance Show SomeCompilerException where
     show (SomeCompilerException e) = show e

 instance Exception SomeCompilerException

 compilerExceptionToException :: Exception e => e -> SomeException
 compilerExceptionToException = toException . SomeCompilerException

 compilerExceptionFromException :: Exception e => SomeException -> Maybe e
 compilerExceptionFromException x = do
     SomeCompilerException a <- fromException x
     cast a

 ---------------------------------------------------------------------
 -- Subhierarchy for exceptions in the compiler frontend

 data SomeFrontendException = forall e . Exception e => SomeFrontendException e
     deriving Typeable

 instance Show SomeFrontendException where
     show (SomeFrontendException e) = show e

 instance Exception SomeFrontendException where
     toException = compilerExceptionToException
     fromException = compilerExceptionFromException

 frontendExceptionToException :: Exception e => e -> SomeException
 frontendExceptionToException = toException . SomeFrontendException

 frontendExceptionFromException :: Exception e => SomeException -> Maybe e
 frontendExceptionFromException x = do
     SomeFrontendException a <- fromException x
     cast a

 ---------------------------------------------------------------------
 -- Specific frontend exceptions

 data IllegalDirectoryStructure = IllegalDirectoryStructure
     deriving (Typeable, Show)

 instance Exception IllegalDirectoryStructure where
     toException   = frontendExceptionToException
     fromException = frontendExceptionFromException

