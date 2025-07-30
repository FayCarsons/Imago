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
    Imago.runFileTransform "TEST.jpeg" program
        >>= either (error . show) (BS.writeFile "FILE-OUTPUT.png")

    putStrLn "File transform OK"

    buf <- BS.readFile "TEST.jpeg"
    Imago.runBufferTransform buf Nothing program
        >>= either (error . show) (BS.writeFile "BUFFER-OUTPUT.png")

