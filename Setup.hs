import Distribution.Simple
import Distribution.Types.HookedBuildInfo
import Distribution.Types.BuildInfo
import Distribution.Types.GenericPackageDescription
import Distribution.Types.PackageDescription
import System.Process
import System.Exit
import System.Environment
import System.Directory

setPkgConfigPath :: IO ()
setPkgConfigPath = do
  homeDir <- getHomeDirectory
  let localPkgConfig = homeDir ++ "/.local/lib/pkgconfig"
  currentPath <- lookupEnv "PKG_CONFIG_PATH"
  let newPath = case currentPath of
        Nothing -> localPkgConfig
        Just path -> localPkgConfig ++ ":" ++ path
  setEnv "PKG_CONFIG_PATH" newPath

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { confHook = \(gpd, hbi) flags -> do
      -- Build Rust library and setup pkg-config before configuration
      putStrLn "Building Rust library..."
      result <- system "./setup.sh"
      case result of
        ExitFailure code -> exitWith (ExitFailure code)
        ExitSuccess -> return ()
      
      -- Set PKG_CONFIG_PATH
      setPkgConfigPath
      
      -- Continue with normal configuration
      confHook simpleUserHooks (gpd, hbi) flags
  }