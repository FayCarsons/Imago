module Main (main) where

import qualified Data.ByteString as BS
import           Graphics.Imago  (Filter (..), TransformM)
import qualified Graphics.Imago  as Imago

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

