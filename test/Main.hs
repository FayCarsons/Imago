module Main (main) where

import qualified Data.ByteString as BS
import Data.Word (Word32)
import Graphics.Imago (Filter (..), TransformM)
import qualified Graphics.Imago as Imago
import qualified Graphics.Imago.Info as Imago

program :: TransformM ()
program = do
  Imago.resize 800 800 Lanczos3 False
  Imago.convert Imago.Png

main :: IO ()
main = do
  Imago.runFileTransform "THUMBNAIL_TEST.png" program
    >>= either (error . show) (BS.writeFile "FILE-OUTPUT.png")

  putStrLn "File transform OK"

  buf <- BS.readFile "THUMBNAIL_TEST.png"
  Imago.runBufferTransform buf (Just Imago.Png) program
    >>= either (error . show) (BS.writeFile "BUFFER-OUTPUT.png")

  putStrLn "Buffer transform OK"

  info <- Imago.getBufferInfo buf >>= either (error . show) return
  let transform = thumbnailTransform (Imago.width info, Imago.height info)
  Imago.runBufferTransform buf Nothing transform
    >>= either (error . show) (BS.writeFile "THUMBNAIL_OUTPUT.webp")

  putStrLn "Thumbnail transform OK"
 where
  ratio :: Word32 -> Float -> Word32
  ratio n r = floor $ r * fromIntegral n
  thumbnailTransform :: (Word32, Word32) -> TransformM ()
  thumbnailTransform (w, h) = do
    Imago.resize (ratio w 0.8) (ratio h 0.8) Imago.Lanczos3 False
    Imago.convert Imago.WebP
