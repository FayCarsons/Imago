module Main (main) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString.Char8 as BS
import Data.Either (isRight)
import Data.Word (Word32)
import Graphics.Imago (Filter (..), TransformM)
import qualified Graphics.Imago as Imago
import qualified Graphics.Imago.Info as Imago
import Test.Hspec

orFail = either (expectationFailure . BS.unpack) (const $ return ())

program :: TransformM ()
program = do
  Imago.resize 800 800 Lanczos3 False
  Imago.convert Imago.Png

main :: IO ()
main = hspec $ do
  describe "Simple Transformations" $ do
    it "Resizes an image (from path) and converts to WebP" $ do
      result <- Imago.runFileTransform "THUMBNAIL_TEST.png" program
      orFail result

    it "Resizes an image (from buffer) and converts to WebP" $ do
      contents <- BS.readFile "THUMBNAIL_TEST.png"
      result <- Imago.runBufferTransform contents (Just Imago.Png) program
      orFail result

  describe "Image information" $ do
    it "Gets image information" $ do
      buf <- liftIO $ BS.readFile "THUMBNAIL_TEST.png"
      info <- Imago.getBufferInfo buf
      orFail info

  describe "Transformations requiring info" $ do
    it "Resizes and converts to WebP, with no initial format" $ do
      buf <- liftIO $ BS.readFile "THUMBNAIL_TEST.png"
      info <- liftIO (Imago.getBufferInfo buf) >>= either (error . BS.unpack) return
      let transform = thumbnailTransform (Imago.width info, Imago.height info)
      result <- Imago.runBufferTransform buf Nothing transform
      orFail result
 where
  ratio :: Word32 -> Float -> Word32
  ratio n r = floor $ r * fromIntegral n
  thumbnailTransform :: (Word32, Word32) -> TransformM ()
  thumbnailTransform (w, h) = do
    Imago.resize (ratio w 0.8) (ratio h 0.8) Imago.Lanczos3 False
    Imago.convert Imago.WebP
