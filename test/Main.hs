{-# LANGUAGE LambdaCase #-}
module Main (main) where

import qualified Data.ByteString as BS
import           Graphics.Imago  (Direction (..), Operation (..),
                                  OutputFormat (..))
import qualified Graphics.Imago  as Imago
import           System.Exit     (exitFailure)

main :: IO ()
main =
    Imago.processImage "glace.png" [Grayscale] Png >>= \case
        Right buffer -> BS.writeFile "OUTPUT.png" buffer
        Left err     -> do
            print err
            exitFailure

