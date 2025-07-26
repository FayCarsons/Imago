{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}

module Graphics.LibImago
    ( ImagoStatus(..)
    , RawOperation(..)
    , RawOutputFormat(..)
    , FilterType(..)
    , Direction(..)
    , Degree(..)
    , rawProcessImage
    , rawProcessBuffer
    , printOperations
    ) where

import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BSU
import           Foreign
import           Foreign.C
import           GHC.IO                 (bracket)

data ImagoStatus
    = Ok
    | InvalidPath
    | InvalidOperation
    | InvalidOutputFormat
    | LoadFailed
    | EncodeFailed
    | UnexpectedNull
    deriving Show

fromInt :: CInt -> ImagoStatus
fromInt = \case
    0  -> Ok
    1 -> InvalidPath
    2 -> InvalidOperation
    3 -> InvalidOutputFormat
    4 -> LoadFailed
    5 -> EncodeFailed
    _  -> error "Invalid ImagoStatus"

toInt :: ImagoStatus -> Integer
toInt = \case
    Ok -> 0
    InvalidPath -> 1
    InvalidOperation -> 2
    InvalidOutputFormat -> 3
    LoadFailed -> 4
    EncodeFailed -> 5
    UnexpectedNull -> 6

instance Storable ImagoStatus where
    sizeOf _ = 4
    alignment _ = 0x4
    peek ptr = do
        val <- peek (castPtr ptr :: Ptr CInt)
        return $ fromInt val
    poke ptr val =
        poke (castPtr ptr :: Ptr CInt)
            $ fromIntegral
            $ toInt val

data RawOutputFormat = RawOutputFormat
    { rofDiscriminant :: CUChar
    , rofQuality      :: CUChar
    }

instance Storable RawOutputFormat where
    sizeOf _ = 2
    alignment _ = 1
    peek ptr = RawOutputFormat <$> peek (castPtr ptr) <*> peek (castPtr ptr)
    poke ptr (RawOutputFormat disc qual) = do
        poke (castPtr ptr) disc
        poke (castPtr ptr) qual

data FilterType
    = Nearest
    | Triangle
    | CatmullRom
    | Gaussian
    | Lanczos3

filterFromInt :: CInt -> FilterType
filterFromInt = \case
    0 -> Nearest
    1 -> Triangle
    2 -> CatmullRom
    3 -> Gaussian
    4 -> Lanczos3
    _ -> error "Invalid FilterType"

filterToInt :: FilterType -> CInt
filterToInt = \case
    Nearest -> 0
    Triangle -> 1
    CatmullRom -> 2
    Gaussian -> 3
    Lanczos3 -> 4

instance Storable FilterType where
    sizeOf _ = 4
    alignment _ = 4
    peek ptr = filterFromInt <$> peek (castPtr ptr)
    poke ptr filterType = poke (castPtr ptr) $ filterToInt filterType

data Direction = Horizontal | Vertical

directionFromInt :: CInt -> Direction
directionFromInt = \case
    0 -> Horizontal
    1 -> Vertical
    _ -> error "Invalid Direction"

directionToInt :: Direction -> CInt
directionToInt = \case
    Horizontal -> 0
    Vertical -> 1

instance Storable Direction where
    sizeOf _ = 4
    alignment _ = 4
    peek ptr = directionFromInt <$> peek (castPtr ptr)
    poke ptr dir = poke (castPtr ptr) $ directionToInt dir

data Degree = Deg90 | Deg180 | Deg270

degreeFromInt :: CInt -> Degree
degreeFromInt = \case
    0 -> Deg90
    1 -> Deg180
    2 -> Deg270
    _ -> error "Invalid Direction"

degreeToInt :: Degree -> CInt
degreeToInt = \case
    Deg90 -> 0
    Deg180 -> 1
    Deg270 -> 2

instance Storable Degree where
    sizeOf _ = 4
    alignment _ = 4
    peek ptr = degreeFromInt <$> peek (castPtr ptr)
    poke ptr dir = poke (castPtr ptr) $ degreeToInt dir

data RawOperation
    = Resize
    { w          :: CInt
    , h          :: CInt
    , filterType :: FilterType
    , exact      :: CBool
    }
    | Rotate Degree
    | Flip Direction
    | Grayscale
    | Blur CFloat
    | Brighten CInt
    | Contrast CFloat

opDiscriminator :: RawOperation -> CInt
opDiscriminator = \case
    Resize {} -> 0
    Rotate _ -> 1
    Flip _ -> 2
    Grayscale -> 3
    Blur _ -> 4
    Brighten _ -> 5
    Contrast _ -> 6

instance Storable RawOperation where
    sizeOf _ = 20
    alignment _ = 4
    peek ptr = do
        discriminant <- peek (castPtr ptr :: Ptr CInt)  -- u8 discriminant
        case discriminant of
            0 -> do  -- Resize
                w <- peekByteOff ptr 4   -- Skip discriminant + padding
                h <- peekByteOff ptr 8
                ft <- peekByteOff ptr 12
                exact <- peekByteOff ptr 16
                return $ Resize w h ft exact
            1 -> do  -- Rotate
                deg <- peekByteOff ptr 4
                return $ Rotate deg
            2 -> do  -- Flip
                dir <- peekByteOff ptr 4
                return $ Flip dir
            3 -> return Grayscale
            4 -> do  -- Blur
                val <- peekByteOff ptr 4
                return $ Blur val
            5 -> do  -- Brighten
                val <- peekByteOff ptr 4
                return $ Brighten val
            6 -> do  -- Contrast
                val <- peekByteOff ptr 4
                return $ Contrast val
            _ -> error "Invalid operation discriminator"
    poke ptr op = do
        fillBytes ptr 0 20
        poke (castPtr ptr :: Ptr CInt) disc

        case op of
            Resize {w, h, filterType, exact} -> do
                pokeByteOff ptr 4 w
                pokeByteOff ptr 8 h
                pokeByteOff ptr 12 filterType
                pokeByteOff ptr 16 exact
            Rotate deg -> pokeByteOff ptr 4 deg
            Flip dir -> pokeByteOff ptr 4 dir
            Grayscale -> return ()
            Blur radius -> pokeByteOff ptr 4 radius
            Brighten factor -> pokeByteOff ptr 4 factor
            Contrast factor -> pokeByteOff ptr 4 factor
        where disc = opDiscriminator op

data Context

foreign import ccall "process_image"
    c_process_image_raw :: Ptr Context -> CString -> Ptr RawOperation -> CSize -> Ptr RawOutputFormat -> IO (Ptr CUChar)

foreign import ccall "process_buffer"
    c_process_buffer_raw :: Ptr Context -> Ptr CChar -> CSize -> Ptr RawOperation -> CSize -> Ptr RawOutputFormat -> IO (Ptr CUChar)

foreign import ccall "make_context"
    c_make_context :: IO (Ptr Context)

foreign import ccall "destroy_context"
    c_destroy_context :: Ptr Context -> IO ()

foreign import ccall "get_status"
    c_get_status :: Ptr Context -> IO CInt

foreign import ccall "get_output_len"
    c_get_output_len :: Ptr Context -> IO CSize

foreign import ccall "destroy_output_buffer"
    c_destroy_output_buffer :: Ptr CUChar -> CSize -> IO ()

foreign import ccall "print_operations"
    c_print_operations :: Ptr RawOperation -> CSize -> IO ()

rawProcessImage :: FilePath -> [RawOperation] -> RawOutputFormat -> IO (Either ImagoStatus ByteString)
rawProcessImage inputPath operations outputFormat = bracket c_make_context c_destroy_context process
    where
        process ctxPtr = do
            contentsPtr <-
                withCString inputPath $ \pathPtr ->
                    withArray operations $ \operationsPtr ->
                        with outputFormat $ \outputFormatPtr ->
                            c_process_image_raw ctxPtr pathPtr operationsPtr (fromIntegral $ length operations) outputFormatPtr

            status <- c_get_status ctxPtr
            case fromInt status of
                Ok -> do
                    if contentsPtr == nullPtr
                        then return $ Left UnexpectedNull
                        else do
                            len <- c_get_output_len ctxPtr
                            contents <- BS.packCStringLen (castPtr contentsPtr, fromIntegral len)
                            _ <- c_destroy_output_buffer contentsPtr len
                            return $ Right contents
                err -> return $ Left err

rawProcessBuffer :: ByteString -> [RawOperation] -> RawOutputFormat -> IO (Either ImagoStatus ByteString)
rawProcessBuffer contents operations outputFormat = bracket c_make_context c_destroy_context process
    where
        process ctxPtr = do
            contentsPtr <-
                BSU.unsafeUseAsCStringLen contents $ \(contentPtr, contentLen) ->
                    withArray operations $ \operationsPtr ->
                        with outputFormat $ \outputFormatPtr ->
                            c_process_buffer_raw ctxPtr contentPtr (fromIntegral contentLen) operationsPtr (fromIntegral $ length operations) outputFormatPtr

            status <- c_get_status ctxPtr
            case fromInt status of
                Ok -> do
                    if contentsPtr == nullPtr
                        then return $ Left UnexpectedNull
                        else do
                            len <- c_get_output_len ctxPtr
                            outputBuffer <- BS.packCStringLen (castPtr contentsPtr, fromIntegral len)
                            _ <- c_destroy_output_buffer contentsPtr len
                            return $ Right outputBuffer
                err -> return $ Left err

-- For debugging marshalling
printOperations :: [RawOperation] -> IO ()
printOperations ops = withArray ops $ \operationsPtr -> c_print_operations operationsPtr (fromIntegral $ length ops)
