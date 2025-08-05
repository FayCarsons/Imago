import Control.Exception
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import System.Directory
import System.Exit
import System.FilePath
import System.Process

main = defaultMainWithHooks simpleUserHooks{preBuild = rustBuild}

rustBuild :: Args -> BuildFlags -> IO HookedBuildInfo
rustBuild args flags = do
  putStrLn "Building Rust library..."

  -- Check if cargo is available
  cargoAvailable <- checkCargoAvailable
  if not cargoAvailable
    then do
      putStrLn "ERROR: cargo not found. Please install Rust toolchain."
      exitFailure
    else return ()

  -- Build the Rust library
  buildRustLibrary

  putStrLn "Rust library build completed."
  return emptyHookedBuildInfo
 where
  checkCargoAvailable = do
    result <- try $ readProcess "cargo" ["--version"] ""
    case result of
      Left (_ :: IOException) -> return False
      Right _ -> return True

buildRustLibrary :: IO ()
buildRustLibrary = do
  -- Change to Core directory and build
  currentDir <- getCurrentDirectory
  let coreDir = currentDir </> "Core"

  coreExists <- doesDirectoryExist coreDir
  if not coreExists
    then do
      putStrLn "ERROR: Core directory not found"
      exitFailure
    else return ()

  setCurrentDirectory coreDir

  -- Run cargo build --release
  exitCode <- rawSystem "cargo" ["build", "--release"]

  -- Return to original directory
  setCurrentDirectory currentDir

  case exitCode of
    ExitSuccess -> return ()
    ExitFailure code -> do
      putStrLn $ "ERROR: Rust build failed with exit code: " ++ show code
      exitFailure
