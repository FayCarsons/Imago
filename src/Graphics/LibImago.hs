{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Graphics.LibImago (
  ImagoStatus (..),
  Direction (..),
  Degree (..),
  Filter (..),
  Format (..),
  COperation (..),
  ImageInfo (..),
  ColorType (..),
  OptionalFormat (..),
  COperation (..),
  rawProcessImage,
  rawProcessBuffer,
  rawGetImageInfo,
  rawGetBufferInfo,
  rawEncodeBuffer,
  rawDecodeBuffer,
  getErrorMessage,
  printOperations,
) where

import Control.Monad ((>=>))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Functor ((<&>))
import Foreign
import Foreign.C
import Graphics.Interface.Raw

type Slice = ForeignPtr RawSlice

foreignSlice :: Ptr RawSlice -> IO Slice
foreignSlice = newForeignPtr c_destroy_output_buffer_ptr

unsafeToByteString :: RawSlice -> IO ByteString
unsafeToByteString (RawSlice len contents) = BS.packCStringLen (castPtr contents, fromIntegral len)

sliceToByteString :: Slice -> IO ByteString
sliceToByteString slice = withForeignPtr slice (peek >=> unsafeToByteString)

data Context

foreign import ccall "process_image"
  c_process_image_raw :: Ptr Context -> CString -> Ptr COperation -> CSize -> IO (Ptr RawSlice)

foreign import ccall "process_buffer"
  c_process_buffer_raw :: Ptr Context -> Ptr CChar -> CSize -> Ptr Format -> Ptr COperation -> CSize -> IO (Ptr RawSlice)

foreign import ccall "make_context"
  c_make_context :: IO (Ptr Context)

foreign import ccall "get_status"
  c_get_status :: Ptr Context -> IO CUChar

foreign import ccall "get_error_message"
  c_get_error_message :: Ptr Context -> IO (Ptr CChar)

foreign import ccall "&destroy_error_message"
  c_destroy_error_message_finalizer :: FunPtr (Ptr CChar -> IO ())

foreign import ccall "&destroy_context"
  c_destroy_context_ptr :: FunPtr (Ptr Context -> IO ())

foreign import ccall "encode_buffer"
  c_encode_buffer :: Ptr Context -> Ptr CUChar -> CSize -> Ptr Format -> IO (Ptr RawSlice)

foreign import ccall "decode_buffer"
  c_decode_buffer :: Ptr Context -> Ptr CUChar -> CSize -> Ptr Format -> IO (Ptr RawSlice)

foreign import ccall "&destroy_output_buffer"
  c_destroy_output_buffer_ptr :: FunPtr (Ptr RawSlice -> IO ())

foreign import ccall "print_operations"
  c_print_operations :: Ptr COperation -> CSize -> IO ()

foreign import ccall "get_image_info"
  c_get_image_info :: Ptr Context -> CString -> IO (Ptr ImageInfo)

foreign import ccall "get_buffer_info"
  c_get_buffer_info :: Ptr Context -> Ptr CUChar -> CSize -> IO (Ptr ImageInfo)

foreign import ccall "&destroy_image_info"
  c_destroy_image_info_finalizer :: FunPtr (Ptr ImageInfo -> IO ())

-- Helpers

-- | Call a function with a managed pointer to the context
withContext :: (Ptr Context -> IO (Ptr a)) -> IO (Either ByteString (Ptr a))
withContext act = do
  context <- c_make_context >>= newForeignPtr c_destroy_context_ptr

  result <- withForeignPtr context act
  status <- fromC <$> withForeignPtr context c_get_status

  case (status, result == nullPtr) of
    (Ok, False) -> pure $ Right result
    _error -> Left <$> withForeignPtr context getErrorMessage

-- | Translate a `Maybe Format` into a pointer that may be null
withOptionalFormat :: Maybe Format -> (Ptr Format -> IO a) -> IO a
withOptionalFormat Nothing action = action nullPtr
withOptionalFormat (Just fmt) action = with fmt action

-- | Low level wrappers
rawProcessImage :: FilePath -> [COperation] -> IO (Either ByteString ByteString)
rawProcessImage inputPath operations = do
  result <-
    withContext $ \ctx ->
      withCString inputPath $ \pathCStr ->
        withArrayLen operations $ \operationsLen operationsPtr ->
          c_process_image_raw ctx pathCStr operationsPtr (fromIntegral operationsLen)

  traverse (foreignSlice >=> sliceToByteString) result

rawProcessBuffer :: ByteString -> Maybe Format -> [COperation] -> IO (Either ByteString ByteString)
rawProcessBuffer buffer optionalFormat operations = do
  result <- withContext $ \ctx ->
    BSU.unsafeUseAsCStringLen buffer $ \(bufPtr, bufLen) ->
      withArrayLen operations $ \operationsLen operationsPtr ->
        withOptionalFormat optionalFormat $ \fmt ->
          c_process_buffer_raw ctx bufPtr (fromIntegral bufLen) fmt operationsPtr (fromIntegral operationsLen)

  traverse (foreignSlice >=> sliceToByteString) result

rawGetImageInfo :: FilePath -> IO (Either ByteString ImageInfo)
rawGetImageInfo path = do
  result <-
    withContext $ \ctx ->
      withCString path $ \pathPtr ->
        c_get_image_info ctx pathPtr

  traverse (newForeignPtr c_destroy_image_info_finalizer >=> flip withForeignPtr peek) result

rawGetBufferInfo :: ByteString -> IO (Either ByteString ImageInfo)
rawGetBufferInfo buffer = do
  result <-
    withContext $ \ctx ->
      BSU.unsafeUseAsCStringLen buffer $ \(bufPtr, bufLen) ->
        c_get_buffer_info ctx (castPtr bufPtr) (fromIntegral bufLen)

  traverse (newForeignPtr c_destroy_image_info_finalizer >=> flip withForeignPtr peek) result

rawEncodeBuffer :: ByteString -> Format -> IO (Either ByteString ByteString)
rawEncodeBuffer buffer encoding = do
  result <-
    withContext $ \ctx ->
      BSU.unsafeUseAsCStringLen buffer $ \(bufPtr, bufLen) ->
        with encoding $ \rawEncoding ->
          c_encode_buffer ctx (castPtr bufPtr) (fromIntegral bufLen) rawEncoding

  traverse (newForeignPtr c_destroy_output_buffer_ptr >=> sliceToByteString) result

rawDecodeBuffer :: ByteString -> Maybe Format -> IO (Either ByteString ByteString)
rawDecodeBuffer buffer optionalFormat = do
  result <-
    withContext $ \ctx ->
      BSU.unsafeUseAsCStringLen buffer $ \(bufPtr, bufLen) ->
        withOptionalFormat optionalFormat $ \fmt ->
          c_decode_buffer ctx (castPtr bufPtr) (fromIntegral bufLen) fmt

  traverse (newForeignPtr c_destroy_output_buffer_ptr >=> sliceToByteString) result

getErrorMessage :: Ptr Context -> IO ByteString
getErrorMessage ctx = do
  errorPtr <- c_get_error_message ctx >>= newForeignPtr c_destroy_error_message_finalizer
  withForeignPtr errorPtr BS.packCString

-- For debugging marshalling
printOperations :: [COperation] -> IO ()
printOperations ops = withArray ops $ \operationsPtr -> c_print_operations operationsPtr (fromIntegral $ length ops)
