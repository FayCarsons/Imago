{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Graphics.Imago
    ( Operation(..)
    , OutputFormat(..)
    , FilterType(..)
    , Direction(..)
    , Degree(..)
    , ImagoStatus(..)
    , processImage
    , processBuffer
    , printOperations
    ) where

import           Data.ByteString   (ByteString)
import           Data.Word         (Word8)
import           Foreign           (fromBool)
import           Graphics.LibImago (Degree (..), Direction (..),
                                    FilterType (..), ImagoStatus (..))
import qualified Graphics.LibImago as LibImago

data Operation
    = Resize
    { w          :: Int
    , h          :: Int
    , filterType :: FilterType
    , exact      :: Bool
    }
    | Rotate Degree
    | Flip Direction
    | Grayscale
    | Blur Float
    | Brighten Int
    | Contrast Float

toRaw :: Operation -> LibImago.RawOperation
toRaw = \case
    Resize{..} -> LibImago.Resize (fromIntegral w) (fromIntegral h) filterType (fromBool exact)
    Rotate deg -> LibImago.Rotate deg
    Flip dir -> LibImago.Flip dir
    Grayscale -> LibImago.Grayscale
    Blur rad -> LibImago.Blur $ realToFrac rad
    Brighten fac -> LibImago.Brighten $ fromIntegral fac
    Contrast fac -> LibImago.Contrast $ realToFrac fac

data OutputFormat
    = WebP
    | Png
    | Jpeg Word8

outputFormatDiscriminator :: Integral a => OutputFormat -> a
outputFormatDiscriminator = \case
    WebP -> 0
    Png -> 1
    Jpeg _ -> 2

toRawFormat :: OutputFormat -> LibImago.RawOutputFormat
toRawFormat = \case
    WebP -> LibImago.RawOutputFormat (outputFormatDiscriminator WebP) 0
    Png -> LibImago.RawOutputFormat (outputFormatDiscriminator Png) 1
    Jpeg q -> LibImago.RawOutputFormat (outputFormatDiscriminator $ Jpeg q) $ fromIntegral q


processImage :: FilePath -> [Operation] -> OutputFormat -> IO (Either ImagoStatus ByteString)
processImage path ops fmt =
    LibImago.rawProcessImage path (map toRaw ops) (toRawFormat fmt)

processBuffer :: ByteString -> [Operation] -> OutputFormat -> IO (Either ImagoStatus ByteString)
processBuffer contents ops fmt = LibImago.rawProcessBuffer contents (map toRaw ops) (toRawFormat fmt)

printOperations :: [Operation] -> IO ()
printOperations = LibImago.printOperations . map toRaw
