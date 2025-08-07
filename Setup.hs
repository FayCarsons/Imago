import Control.Exception
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program.Find (defaultProgramSearchPath, findProgramOnSearchPath)
import Distribution.Simple.Setup
import Distribution.Simple.Utils (maybeExit, rawSystemStdInOut)
import Distribution.Utils.IOData (IODataMode (..))
import Distribution.Verbosity (Verbosity)
import qualified Distribution.Verbosity as Verbosity
import System.Directory
import System.Exit
import System.FilePath
import System.Process

main :: IO ()
main = defaultMainWithHooks hooks
 where
  hooks =
    simpleUserHooks
      { preConf = \_ flags -> do
          rsMake (fromFlag $ configVerbosity flags)
          pure emptyHookedBuildInfo
      , confHook = \a flags ->
          confHook simpleUserHooks a flags >>= rsAddDirs
      , postClean = \_ flags _ _ ->
          rsClean (fromFlag $ cleanVerbosity flags)
      }

rsFolder :: FilePath
rsFolder = "Core"

execCargo :: Verbosity -> String -> [String] -> IO ()
execCargo verbosity command args = do
  cargoPath <- findProgramOnSearchPath Verbosity.silent defaultProgramSearchPath "cargo"
  dir <- getCurrentDirectory
  let cargoExec =
        case cargoPath of
          Just (p, _) -> p
          Nothing -> "cargo"
      cargoArgs = command : args
      workingDir = Just (dir </> rsFolder)
      thirdComponent (_, _, c) = c
  maybeExit . fmap thirdComponent $ rawSystemStdInOut verbosity cargoExec cargoArgs workingDir Nothing Nothing IODataModeBinary

rsMake :: Verbosity -> IO ()
rsMake verbosity = execCargo verbosity "build" ["--release", "--lib"]

rsAddDirs :: LocalBuildInfo -> IO LocalBuildInfo
rsAddDirs lbi' = do
  dir <- getCurrentDirectory
  let rustIncludeDir = dir </> rsFolder </> "include"
      rustLibDir = dir </> rsFolder </> "target/release"
      updatePkgDescr pkgDescr = pkgDescr{library = updateLib <$> library pkgDescr}
      updateLib lib = lib{libBuildInfo = updateLibBi (libBuildInfo lib)}
      updateLibBi libBuild =
        libBuild
          { includeDirs = rustIncludeDir : includeDirs libBuild
          , extraLibDirs = rustLibDir : extraLibDirs libBuild
          }
      updateLbi lbi = lbi{localPkgDescr = updatePkgDescr (localPkgDescr lbi)}
  pure $ updateLbi lbi'

rsClean :: Verbosity -> IO ()
rsClean verbosity = execCargo verbosity "clean" []
