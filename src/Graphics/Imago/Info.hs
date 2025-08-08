module Graphics.Imago.Info (
  ImageInfo (..),
  getFileInfo,
  getBufferInfo,
  dimensions,
  format,
  colorType,
  size,
  hasAlpha,
  aspectRatio,
  ColorType (..),
) where

import Data.ByteString (ByteString)
import Foreign (toBool)
import Graphics.Interface.Raw (
  ColorType (..),
  Format,
  ImageInfo,
  OptionalFormat (OptionalFormat),
 )
import qualified Graphics.Interface.Raw as Raw
import Graphics.LibImago (rawGetBufferInfo, rawGetImageInfo)

-- | Get Image information from a file
getFileInfo :: FilePath -> IO (Either ByteString ImageInfo)
getFileInfo = rawGetImageInfo

-- | Get Image information from a buffer
getBufferInfo :: ByteString -> IO (Either ByteString ImageInfo)
getBufferInfo = rawGetBufferInfo

-- | Get image dimensions as a tuple
dimensions :: ImageInfo -> (Int, Int)
dimensions info = (fromIntegral w, fromIntegral h)
 where
  w = Raw.width info
  h = Raw.height info

-- | Get image format (i.e. JPEG, AVIF, WEBP)
format :: ImageInfo -> Maybe Format
format = toMaybe . Raw.rawFormat
 where
  toMaybe (OptionalFormat 1 fmt) = Just fmt
  toMaybe _ = Nothing

-- | Get color type (i.e. Rgb16, Rgba32F)
colorType :: ImageInfo -> ColorType
colorType = Raw.color

-- | Get file or buffer size in bytes
size :: ImageInfo -> Int
size = fromIntegral . Raw.fileSize

-- | Whether the image's color type has an alpha channel
hasAlpha :: ImageInfo -> Bool
hasAlpha = toBool . Raw.hasAlpha

-- | Get image's aspect ratio
aspectRatio :: ImageInfo -> Double
aspectRatio = realToFrac . Raw.aspectRatio
