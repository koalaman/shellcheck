import Distribution.PackageDescription (
  HookedBuildInfo,
  emptyHookedBuildInfo )
import Distribution.Simple (
  Args,
  UserHooks ( preSDist ),
  defaultMainWithHooks,
  simpleUserHooks )
import Distribution.Simple.Setup ( SDistFlags )

-- | This requires the process package from,
--
--   https://hackage.haskell.org/package/process
--
import System.Process ( callCommand )


-- | This will use almost the default implementation, except we switch
--   out the default pre-sdist hook with our own, 'myPreSDist'.
--
main = defaultMainWithHooks myHooks
  where
    myHooks = simpleUserHooks { preSDist = myPreSDist }


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
  putStrLn "Building the man page..."
  putStrLn pandoc_cmd
  callCommand pandoc_cmd
  return emptyHookedBuildInfo
  where
    pandoc_cmd = "pandoc -s -t man shellcheck.1.md -o shellcheck.1"
