{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
module Graphics.Interface.Raw where 

#include "libimago.h"

import Foreign
import Foreign.C.Types

-- Named `ByteArray` in the header file
data RawSlice
    = RawSlice
    { len      :: CSize
    , contents :: Ptr CUChar
    } deriving Show

instance Storable RawSlice where 
    sizeOf _ = #{size struct ByteArray}
    alignment _ = #{alignment struct ByteArray}
    peek ptr = do 
        len <- #{peek struct ByteArray, len} ptr :: IO CSize
        contents <- #{peek struct ByteArray, data} ptr :: IO (Ptr CUChar)
        return $ RawSlice len contents
    poke ptr (RawSlice len contents) = do 
        #{poke struct ByteArray, len} ptr len
        #{poke struct ByteArray, data} ptr contents

class CEnum a where 
    fromC :: CUChar -> a
    toC :: a -> CUChar


data ImagoStatus 
    = Ok 
    | InvalidPath
    | InvalidInputBuffer
    | InvalidOperation
    | InvalidInputFormat 
    | InvalidOutputFormat
    | LoadFailed
    | EncodeFailed
    | NullContext
    deriving (Show, Eq, Ord)

instance CEnum ImagoStatus where 
    fromC = \case 
        #{const OK} -> Ok 
        #{const InvalidPath} -> InvalidPath
        #{const InvalidInputBuffer} -> InvalidInputBuffer
        #{const InvalidOperation} -> InvalidOperation
        #{const InvalidInputFormat} -> InvalidInputFormat
        #{const InvalidOutputFormat} -> InvalidOutputFormat
        #{const LoadFailed} -> LoadFailed
        #{const EncodeFailed} -> EncodeFailed
        #{const NullContext} -> NullContext
        tag -> error $ "Unknown Imago Status tag: " ++ show tag
    toC = \case 
        Ok -> #{const OK}
        InvalidPath -> #{const InvalidPath}
        InvalidInputBuffer -> #{const InvalidInputBuffer}
        InvalidOperation -> #{const InvalidOperation}
        InvalidInputFormat -> #{const InvalidInputFormat}
        InvalidOutputFormat -> #{const InvalidOutputFormat}
        LoadFailed -> #{const LoadFailed}
        EncodeFailed -> #{const EncodeFailed}
        NullContext -> #{const NullContext}

instance Storable ImagoStatus where 
    sizeOf _ = sizeOf (0 :: CUChar)
    alignment _ = alignment (0 :: CUChar)
    peek ptr = fromC <$> peek (castPtr ptr)
    poke ptr val = poke (castPtr ptr) (toC val)

data Degree 
    = Deg90
    | Deg180
    | Deg270
    deriving (Show, Eq, Ord)

instance CEnum Degree where 
    fromC = \case 
        #{const Deg90} -> Deg90
        #{const Deg180} -> Deg180
        #{const Deg270} -> Deg270
        tag -> error $ "Unknown 'Degree' tag: " ++ show tag
    toC = \case 
        Deg90 -> #{const Deg90}
        Deg180 -> #{const Deg180}
        Deg270 -> #{const Deg270}        

instance Storable Degree where 
    sizeOf _ = sizeOf (0 :: CUChar)
    alignment _ = alignment (0 :: CUChar)
    peek ptr = fromC <$> peek (castPtr ptr)
    poke ptr val = poke (castPtr ptr) (toC val)

data Direction = Horizontal | Vertical deriving (Show, Eq, Ord)

instance CEnum Direction where 
    fromC = \case 
        #{const Horizontal} -> Horizontal
        #{const Vertical} -> Vertical
        tag -> error $ "Invalid direction tag: " ++ show tag
    toC = \case 
        Horizontal -> #{const Horizontal}
        Vertical -> #{const Vertical}

instance Storable Direction where 
    sizeOf _ = sizeOf (0 :: CUChar)
    alignment _ = alignment (0 :: CUChar)
    peek ptr = fromC <$> peek (castPtr ptr)
    poke ptr val = poke (castPtr ptr) (toC val)

data Filter 
    = Nearest 
    | Triangle 
    | CatmullRom 
    | Gaussian
    | Lanczos3 
    deriving (Show, Eq, Ord)

instance CEnum Filter where 
    fromC = \case 
        #{const Nearest} -> Nearest
        #{const Triangle} -> Triangle
        #{const CatmullRom} -> CatmullRom
        #{const Gaussian} -> Gaussian
        #{const Lanczos3} -> Lanczos3
        tag -> error $ "Unknown 'Filter' tag: " ++ show tag
    toC = \case 
        Nearest -> #{const Nearest}
        Triangle -> #{const Triangle}
        CatmullRom -> #{const CatmullRom}
        Gaussian -> #{const Gaussian}
        Lanczos3 -> #{const Lanczos3}

instance Storable Filter where 
    sizeOf _ = sizeOf (0 :: CUChar)
    alignment _ = alignment (0 :: CUChar)
    peek ptr = fromC <$> peek (castPtr ptr)
    poke ptr val = poke (castPtr ptr) (toC val)

data COperation
    = CResize Word32 Word32 Filter CBool
    | CRotate Degree 
    | CFlip Direction
    | CBlur CFloat
    | CGrayScale
    | CBrighten CInt
    | CContrast CFloat
    deriving (Show, Eq, Ord)

operationTag :: COperation -> CUChar
operationTag = \case
    CResize _ _ _ _ -> #{const Resize}
    CRotate _ -> #{const Rotate}
    CFlip _ -> #{const Flip}
    CBlur _ -> #{const Blur}
    CGrayScale -> #{const GrayScale}
    CBrighten _ -> #{const Brighten} 
    CContrast _ -> #{const Contrast}

instance Storable COperation where 
    sizeOf _ = #{size struct Operation}
    alignment _ = #{alignment struct Operation}
    peek ptr = do 
        tag <- #{peek struct Operation, tag} ptr :: IO CUChar
        case tag of 
            #{const Resize} -> do 
                w <- #{peek struct Operation, resize.w} ptr :: IO Word32
                h <- #{peek struct Operation, resize.h} ptr :: IO Word32
                filterType <- #{peek struct Operation, resize.filter} ptr :: IO Filter
                exact <- #{peek struct Operation, resize.exact} ptr :: IO CBool
                return $ CResize w h filterType exact
            #{const Rotate} -> 
                CRotate <$> (#{peek struct Operation, rotate} ptr :: IO Degree)
            #{const Flip} -> 
                CFlip <$> (#{peek struct Operation, flip} ptr :: IO Direction)
            #{const GrayScale} -> return CGrayScale
            #{const Blur} -> CBlur <$> (#{peek struct Operation, blur} ptr :: IO CFloat)
            #{const Brighten} -> CBrighten <$> (#{peek struct Operation, brighten} ptr :: IO CInt)
            #{const Contrast} -> CContrast <$> (#{peek struct Operation, contrast} ptr :: IO CFloat)
            tag -> error $ "Unknown operation tag: " ++ show tag
    poke ptr op = do 
        #{poke struct Operation, tag} ptr (operationTag op)
        case op of 
            CResize w h filterType exact -> do 
                #{poke struct Operation, resize.w} ptr w
                #{poke struct Operation, resize.h} ptr h
                #{poke struct Operation, resize.filter} ptr filterType
                #{poke struct Operation, resize.exact} ptr exact
            CRotate degree ->
                #{poke struct Operation, rotate} ptr degree
            CFlip direction ->
                #{poke struct Operation, flip} ptr direction
            CGrayScale -> return ()
            CBlur amount ->
                #{poke struct Operation, blur} ptr amount
            CBrighten brightness ->
                #{poke struct Operation, brighten} ptr brightness
            CContrast contrast ->
                #{poke struct Operation, contrast} ptr contrast

data Format 
    = WebP 
    | Png 
    | Jpeg Word8
    deriving (Show, Eq, Ord)

formatTag :: Format -> CUChar 
formatTag = \case 
    WebP -> #{const WebP}
    Png -> #{const Png}
    Jpeg _ -> #{const Jpeg}

instance Storable Format where 
    sizeOf _ = #{size struct Format}
    alignment _ = #{alignment struct Format}
    peek ptr = do 
        tag <- #{peek struct Format, tag} ptr :: IO CUChar
        case tag of 
            #{const WebP} -> return WebP
            #{const Png} -> return Png
            #{const Jpeg} -> do 
                Jpeg <$> (#{peek struct Format, jpeg} ptr :: IO Word8)
            _ -> error $ "Unknown 'Format' tag: " ++ show tag
    poke ptr fmt = do 
        #{poke struct Format, tag} ptr (formatTag fmt)
        case fmt of 
            Jpeg quality -> #{poke struct Format, jpeg} ptr quality 
            _ -> return ()