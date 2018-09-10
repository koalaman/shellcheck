{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Distribution.PackageDescription (
  HookedBuildInfo,
  emptyHookedBuildInfo )
import Distribution.Simple (
  Args,
  UserHooks ( preSDist ),
  defaultMainWithHooks,
  simpleUserHooks )
import Distribution.Simple.Setup ( SDistFlags )

import System.Process ( system )
import System.Directory ( doesFileExist, getModificationTime )

#ifndef MIN_VERSION_cabal_doctest
#define MIN_VERSION_cabal_doctest(x,y,z) 0
#endif

#if MIN_VERSION_cabal_doctest(1,0,0)

import Distribution.Extra.Doctest ( addDoctestsUserHook )
main :: IO ()
main = defaultMainWithHooks $ addDoctestsUserHook "doctests" myHooks
  where
    myHooks = simpleUserHooks { preSDist = myPreSDist }

#else

#ifdef MIN_VERSION_Cabal
-- If the macro is defined, we have new cabal-install,
-- but for some reason we don't have cabal-doctest in package-db
--
-- Probably we are running cabal sdist, when otherwise using new-build
-- workflow
#warning You are configuring this package without cabal-doctest installed. \
         The doctests test-suite will not work as a result. \
         To fix this, install cabal-doctest before configuring.
#endif

main :: IO ()
main = defaultMainWithHooks myHooks
  where
    myHooks = simpleUserHooks { preSDist = myPreSDist }

#endif



-- | This hook will be executed before e.g. @cabal sdist@. It runs
--   pandoc to create the man page from shellcheck.1.md. If the pandoc
--   command is not found, this will fail with an error message:
--
--     /bin/sh: pandoc: command not found
--
--   Since the man page is listed in the Extra-Source-Files section of
--   our cabal file, a failure here should result in a failure to
--   create the distribution tarball (that's a good thing).
--
myPreSDist :: Args -> SDistFlags -> IO HookedBuildInfo
myPreSDist _ _ = do
  exists <- doesFileExist "shellcheck.1"
  if exists
  then do
    source <- getModificationTime "shellcheck.1.md"
    target <- getModificationTime "shellcheck.1"
    if target < source
    then makeManPage
    else putStrLn "shellcheck.1 is more recent than shellcheck.1.md"
  else makeManPage
  return emptyHookedBuildInfo
  where
    makeManPage = do
      putStrLn "Building the man page (shellcheck.1) with pandoc..."
      putStrLn pandoc_cmd
      result <- system pandoc_cmd
      putStrLn $ "pandoc exited with " ++ show result
    pandoc_cmd = "pandoc -s -t man shellcheck.1.md -o shellcheck.1"
