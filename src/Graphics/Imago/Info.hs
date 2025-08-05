module Graphics.Imago.Info (ImageInfo (..), getImageInfo, getBufferInfo, dimensions, size, format) where

import Data.ByteString (ByteString)
import Graphics.Interface.Raw (
  Format,
  ImageInfo (..),
  OptionalFormat (OptionalFormat),
 )
import Graphics.LibImago (rawGetBufferInfo, rawGetImageInfo)

getImageInfo :: FilePath -> IO (Either ByteString ImageInfo)
getImageInfo = rawGetImageInfo

getBufferInfo :: ByteString -> IO (Either ByteString ImageInfo)
getBufferInfo = rawGetBufferInfo

dimensions :: ImageInfo -> (Int, Int)
dimensions info = (fromIntegral w, fromIntegral h)
 where
  w = width info
  h = height info

size :: ImageInfo -> Int
size = fromIntegral . fileSize

format :: ImageInfo -> Maybe Format
format = toMaybe . Graphics.Interface.Raw.rawFormat
 where
  toMaybe (OptionalFormat 1 fmt) = Just fmt
  toMaybe _ = Nothing
