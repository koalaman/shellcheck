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
  putStrLn "Building the man page (shellcheck.1) with pandoc..."
  putStrLn pandoc_cmd
  result <- system pandoc_cmd
  putStrLn $ "pandoc exited with " ++ show result
  return emptyHookedBuildInfo
  where
    pandoc_cmd = "pandoc -s -t man shellcheck.1.md -o shellcheck.1"
