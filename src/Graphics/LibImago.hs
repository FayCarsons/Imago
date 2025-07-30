{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Graphics.LibImago
    ( ImagoStatus(..)
    , Direction(..)
    , Degree(..)
    , Filter(..)
    , Format(..)
    , COperation(..)
    , ImageInfo(..)
    , ColorType(..)
    , OptionalFormat(..)
    , rawProcessImage
    , rawProcessBuffer
    , rawGetImageInfo
    , rawGetBufferInfo
    , getErrorMessage
    , printOperations
    ) where

import           Control.Monad          ((>=>))
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BSU
import           Data.Functor           ((<&>))
import           Foreign
import           Foreign.C
import           Graphics.Interface.Raw

type Slice = ForeignPtr RawSlice

foreignSlice :: Ptr RawSlice -> IO Slice
foreignSlice = newForeignPtr c_destroy_output_buffer_ptr

unsafeToByteString :: RawSlice -> IO ByteString
unsafeToByteString (RawSlice len contents) = BS.packCStringLen (castPtr contents, fromIntegral len)

sliceToByteString :: Slice -> IO ByteString
sliceToByteString slice = withForeignPtr slice (peek >=> unsafeToByteString)

data Context

foreign import ccall "process_image"
    c_process_image_raw :: Ptr Context -> CString -> Ptr COperation -> CSize -> Ptr Format -> IO (Ptr RawSlice)

foreign import ccall "process_buffer"
    c_process_buffer_raw :: Ptr Context -> Ptr CChar -> CSize -> Ptr Format -> Ptr COperation -> CSize -> Ptr Format -> IO (Ptr RawSlice)

foreign import ccall "make_context"
    c_make_context :: IO (Ptr Context)

foreign import ccall "get_status"
    c_get_status :: Ptr Context -> IO CUChar

foreign import ccall "get_error_message"
    c_get_error_message :: Ptr Context -> IO (Ptr CChar)

foreign import ccall "&destroy_context"
    c_destroy_context_ptr :: FunPtr (Ptr Context -> IO ())

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

rawProcessImage :: FilePath -> [COperation] -> Format -> IO (Either ByteString ByteString)
rawProcessImage inputPath operations outputFormat = do
    context <- c_make_context >>= newForeignPtr c_destroy_context_ptr

    result <- withCString inputPath $ \pathCStr ->
                withArray operations $ \opsArray ->
                    with outputFormat $ \formatPtr ->
                        withForeignPtr context $ \ctx ->
                                c_process_image_raw ctx pathCStr opsArray
                                                    (fromIntegral $ length operations)
                                                    formatPtr

    status <- fromC <$> withForeignPtr context c_get_status
    case (status, result == nullPtr) of
        (Ok, False) -> foreignSlice result >>= sliceToByteString <&> Right
        _           -> Left <$> withForeignPtr context getErrorMessage

rawProcessBuffer :: ByteString -> Maybe Format -> [COperation] -> Format -> IO (Either ByteString ByteString)
rawProcessBuffer contents mFormat operations outputFormat = do
    -- Create context with ForeignPtr
    context <- c_make_context >>= newForeignPtr c_destroy_context_ptr

    result <-
        BSU.unsafeUseAsCStringLen contents $ \(contentPtr, contentLen) ->
            withArray operations $ \operationsPtr ->
                with outputFormat $ \outputFormatPtr ->
                    withForeignPtr context $ \ctx ->
                        case mFormat of
                            Just format ->
                                with format $ \inputFormat ->
                                    c_process_buffer_raw
                                        ctx
                                        contentPtr
                                        (fromIntegral contentLen)
                                        inputFormat
                                        operationsPtr
                                        (fromIntegral $ length operations)
                                        outputFormatPtr
                            Nothing ->
                                    c_process_buffer_raw
                                        ctx
                                        contentPtr
                                        (fromIntegral contentLen)
                                        nullPtr
                                        operationsPtr
                                        (fromIntegral $ length operations)
                                        outputFormatPtr

    status <- fromC <$> withForeignPtr context c_get_status
    case (status, result == nullPtr) of
        (Ok, False) -> foreignSlice result >>= sliceToByteString <&> Right
        _           -> Left <$> withForeignPtr context getErrorMessage

rawGetImageInfo :: FilePath -> IO (Either ByteString ImageInfo)
rawGetImageInfo path = do
    context <- c_make_context >>= newForeignPtr c_destroy_context_ptr

    result <-
        withCString path $ \pathPtr ->
            withForeignPtr context $ \ctx ->
                c_get_image_info ctx pathPtr

    status <- fromC <$> withForeignPtr context c_get_status
    case (status, result == nullPtr) of
        (Ok, False) -> do
            -- Create ForeignPtr with finalizer for automatic cleanup
            infoPtr <- newForeignPtr c_destroy_image_info_finalizer result
            info <- withForeignPtr infoPtr peek
            return $ Right info
        _  -> Left <$> withForeignPtr context getErrorMessage

rawGetBufferInfo :: ByteString -> IO (Either ByteString ImageInfo)
rawGetBufferInfo buffer = do
    context <- c_make_context >>= newForeignPtr c_destroy_context_ptr

    result <-
        BSU.unsafeUseAsCStringLen buffer $ \(bufferPtr, bufferLen) ->
            withForeignPtr context $ \ctx ->
                c_get_buffer_info ctx (castPtr bufferPtr) (fromIntegral bufferLen)

    status <- fromC <$> withForeignPtr context c_get_status
    case (status, result == nullPtr) of
        (Ok, False) -> do
            -- Create ForeignPtr with finalizer for automatic cleanup
            infoPtr <- newForeignPtr c_destroy_image_info_finalizer result
            info <- withForeignPtr infoPtr peek
            return $ Right info
        _  -> Left <$> withForeignPtr context getErrorMessage

getErrorMessage :: Ptr Context -> IO ByteString
getErrorMessage ctx = do
    errorPtr <- c_get_error_message ctx
    if errorPtr == nullPtr
        then return BS.empty
        else BS.packCString (castPtr errorPtr)

-- For debugging marshalling
printOperations :: [COperation] -> IO ()
printOperations ops = withArray ops $ \operationsPtr -> c_print_operations operationsPtr (fromIntegral $ length ops)
