{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.LibImago
    ( ImagoStatus(..)
    , Direction(..)
    , Degree(..)
    , Filter(..)
    , Format(..)
    , COperation(..)
    , rawProcessImage
    , rawProcessBuffer
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
    c_process_buffer_raw :: Ptr Context -> Ptr CChar -> CSize -> Ptr COperation -> CSize -> Ptr Format -> IO (Ptr RawSlice)

foreign import ccall "make_context"
    c_make_context :: IO (Ptr Context)

foreign import ccall "get_status"
    c_get_status :: Ptr Context -> IO CUChar

foreign import ccall "&destroy_context"
    c_destroy_context_ptr :: FunPtr (Ptr Context -> IO ())

foreign import ccall "&destroy_output_buffer"
    c_destroy_output_buffer_ptr :: FunPtr (Ptr RawSlice -> IO ())

foreign import ccall "print_operations"
    c_print_operations :: Ptr COperation -> CSize -> IO ()

rawProcessImage :: FilePath -> [COperation] -> Format -> IO (Either ImagoStatus ByteString)
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
        (Ok, True)  -> return $ Left NullContext
        (err, _)    -> return $ Left err

rawProcessBuffer :: ByteString -> [COperation] -> Format -> IO (Either ImagoStatus ByteString)
rawProcessBuffer contents operations outputFormat = do
    -- Create context with ForeignPtr
    context <- c_make_context >>= newForeignPtr c_destroy_context_ptr

    result <-
        BSU.unsafeUseAsCStringLen contents $ \(contentPtr, contentLen) ->
            withArray operations $ \operationsPtr ->
                with outputFormat $ \outputFormatPtr ->
                    withForeignPtr context $ \ctx ->
                        c_process_buffer_raw
                            ctx
                            contentPtr
                            (fromIntegral contentLen)
                            operationsPtr
                            (fromIntegral $ length operations)
                            outputFormatPtr

    status <- fromC <$> withForeignPtr context c_get_status
    case (status, result == nullPtr) of
        (Ok, False) -> foreignSlice result >>= sliceToByteString <&> Right
        (Ok, True)  -> return $ Left NullContext
        (err, _)    -> return $ Left err

-- For debugging marshalling
printOperations :: [COperation] -> IO ()
printOperations ops = withArray ops $ \operationsPtr -> c_print_operations operationsPtr (fromIntegral $ length ops)
