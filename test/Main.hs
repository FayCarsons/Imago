module Main (main) where

import qualified Data.ByteString as BS
import           Graphics.Imago  (Filter (..), Format (..), TransformM)
import qualified Graphics.Imago  as Imago

program :: TransformM ()
program = do
    Imago.resize 400 400 Nearest False

main :: IO ()
main =
    Imago.runFileTransform "Glace.png" Png program
        >>= either ( error . show ) (BS.writeFile "OUTPUT.png")
