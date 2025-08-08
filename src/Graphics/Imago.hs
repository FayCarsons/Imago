{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Graphics.Imago (
  -- Parameters
  Degree (..),
  Direction (..),
  Filter (..),
  Format (..),
  -- Image processing program
  Program,
  resize,
  rotate,
  flip,
  grayscale,
  blur,
  brighten,
  contrast,
  quality,
  convert,
  -- Run a program on a buffer or file
  runFileTransform,
  runBufferTransform,
  decodePixelData,
  decodePixelDataWithFormat,
  encodePixelData,
) where

import Control.Monad.Free (Free (..), MonadFree, liftF)
import Control.Monad.Free.TH
import Data.Binary (Word32, Word8)
import Data.ByteString (ByteString)
import Foreign (fromBool)
import Graphics.LibImago
import Prelude hiding (flip)

data Transform a
  = Resize
      { w :: Word32
      , h :: Word32
      , filterType :: Filter
      , exact :: Bool
      , next :: a
      }
  | Rotate Degree a
  | Flip Direction a
  | Grayscale a
  | Blur Float a
  | Brighten Int a
  | Contrast Float a
  | Quality Word8 a
  | Convert Format a
  deriving (Show, Eq, Ord, Functor)

makeFree ''Transform

type TransformM = Free Transform

{- | An image transformation
This is a free monad, so we can express image transformations as monadic `do` statements,
use `bind`/`>>=`, conditionally apply operations.

Example:
```
myImageTransform = do
  resize 400 400 Nearest False
  quality 90
  convert Avif

anotherTransform = convert Jpeg

imageIdentity = pure ()
```
-}
type Program = TransformM ()

unfold :: TransformM a -> [COperation]
unfold (Pure _) = []
unfold (Free op) =
  case op of
    Resize w h filterType exact next -> CResize (fromIntegral w) (fromIntegral h) filterType (fromBool exact) : unfold next
    Rotate deg next -> CRotate deg : unfold next
    Flip dir next -> CFlip dir : unfold next
    Grayscale next -> CGrayScale : unfold next
    Blur rad next -> CBlur (realToFrac rad) : unfold next
    Brighten fac next -> CBrighten (fromIntegral fac) : unfold next
    Contrast fac next -> CContrast (realToFrac fac) : unfold next
    Quality qual next -> CQuality (fromIntegral qual) : unfold next
    Convert format next -> CConvert format : unfold next

-- | Run a transformation on a file
runFileTransform :: FilePath -> Program -> IO (Either ByteString ByteString)
runFileTransform path transform =
  rawProcessImage path (unfold transform)

-- | Run a transformation on a buffer
runBufferTransform :: ByteString -> Maybe Format -> Program -> IO (Either ByteString ByteString)
runBufferTransform contents inputFormat transform =
  rawProcessBuffer contents inputFormat (unfold transform)

-- | Decode a buffer into its pixel data (layout is determined by its `ColorType`, which can be accessed via `getFileInfo` or `getBufferInfo`)
decodePixelData :: ByteString -> IO (Either ByteString ByteString)
decodePixelData buffer = rawDecodeBuffer buffer Nothing

-- | Decode a buffer into its pixel data, with a format hint
decodePixelDataWithFormat :: ByteString -> Format -> IO (Either ByteString ByteString)
decodePixelDataWithFormat buffer format = rawDecodeBuffer buffer (Just format)

-- | Encode raw pixel data into a given format
encodePixelData :: ByteString -> Format -> IO (Either ByteString ByteString)
encodePixelData = rawEncodeBuffer
