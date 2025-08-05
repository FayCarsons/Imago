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
    | UnsupportedImageFormat
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
      #{const UnsupportedImageFormat} -> UnsupportedImageFormat
      tag -> error $ "Unknown Imago Status tag: " ++ show tag
    toC = \case 
      Ok -> #{const OK}
      InvalidPath -> #{const InvalidPath}
      InvalidInputBuffer -> #{const InvalidInputBuffer}
      InvalidOperation -> #{const InvalidOperation}
      InvalidInputFormat -> #{const InvalidInputFormat}
      InvalidOutputFormat -> #{const InvalidOutputFormat}
      UnsupportedImageFormat -> #{const UnsupportedImageFormat}
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
    | CQuality CUChar
    | CConvert Format
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
  CQuality _ -> #{const Quality}
  CConvert _ -> #{const Convert}

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
            #{const Quality} -> CQuality <$> (#{peek struct Operation, quality} ptr :: IO CUChar)
            #{const Convert} -> CConvert <$> (#{peek struct Operation, convert} ptr :: IO Format)
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
            CQuality quality ->
                #{poke struct Operation, quality} ptr quality
            CConvert format -> 
                #{poke struct Operation, convert} ptr format

data Format 
    = Avif
    | Bmp
    | Dds
    | Farbfeld
    | Gif
    | Hdr
    | Ico
    | Jpeg
    | OpenExr
    | Pcx
    | Png
    | Pnm
    | Qoi
    | Tga
    | Tiff
    | WebP
    deriving (Show, Eq, Ord)

instance CEnum Format where 
    fromC = \case 
      #{const Avif} -> Avif
      #{const Bmp} -> Bmp
      #{const Dds} -> Dds
      #{const Farbfeld} -> Farbfeld
      #{const Gif} -> Gif
      #{const Hdr} -> Hdr
      #{const Ico} -> Ico
      #{const Jpeg} -> Jpeg
      #{const OpenExr} -> OpenExr
      #{const Pcx} -> Pcx
      #{const Png} -> Png
      #{const Pnm} -> Pnm
      #{const Qoi} -> Qoi
      #{const Tga} -> Tga
      #{const Tiff} -> Tiff
      #{const WebP} -> WebP
      tag -> error $ "Unknown 'Format' tag: " ++ show tag
    toC = \case 
      Avif -> #{const Avif}
      Bmp -> #{const Bmp}
      Dds -> #{const Dds}
      Farbfeld -> #{const Farbfeld}
      Gif -> #{const Gif}
      Hdr -> #{const Hdr}
      Ico -> #{const Ico}
      Jpeg -> #{const Jpeg}
      OpenExr -> #{const OpenExr}
      Pcx -> #{const Pcx}
      Png -> #{const Png}
      Pnm -> #{const Pnm}
      Qoi -> #{const Qoi}
      Tga -> #{const Tga}
      Tiff -> #{const Tiff}
      WebP -> #{const WebP}

instance Storable Format where 
    sizeOf _ = sizeOf (0 :: CUChar)
    alignment _ = alignment (0 :: CUChar)
    peek ptr = fromC <$> peek (castPtr ptr)
    poke ptr val = poke (castPtr ptr) (toC val)

data ColorType
    = L8
    | L16
    | La8
    | La16
    | Rgb8
    | Rgb16
    | Rgb32F
    | Rgba8
    | Rgba16
    | Rgba32F
    deriving (Show, Eq, Ord)

instance CEnum ColorType where 
    fromC = \case 
      #{const L8} -> L8
      #{const L16} -> L16
      #{const La8} -> La8
      #{const La16} -> La16
      #{const Rgb8} -> Rgb8
      #{const Rgb16} -> Rgb16
      #{const Rgb32F} -> Rgb32F
      #{const Rgba8} -> Rgba8
      #{const Rgba16} -> Rgba16
      #{const Rgba32F} -> Rgba32F
      tag -> error $ "Unknown 'ColorType' tag: " ++ show tag
    toC = \case 
      L8 -> #{const L8}
      L16 -> #{const L16}
      La8 -> #{const La8}
      La16 -> #{const La16}
      Rgb8 -> #{const Rgb8}
      Rgb16 -> #{const Rgb16}
      Rgb32F -> #{const Rgb32F}
      Rgba8 -> #{const Rgba8}
      Rgba16 -> #{const Rgba16}
      Rgba32F -> #{const Rgba32F}

instance Storable ColorType where 
    sizeOf _ = sizeOf (0 :: CUChar)
    alignment _ = alignment (0 :: CUChar)
    peek ptr = fromC <$> peek (castPtr ptr)
    poke ptr val = poke (castPtr ptr) (toC val)

data OptionalFormat 
    = OptionalFormat 
    { has_value :: CBool 
    , value :: Format 
    } deriving Show

instance Storable OptionalFormat where 
    sizeOf _ = #{size struct OptionalFormat}
    alignment _ = #{alignment struct OptionalFormat}
    peek ptr = OptionalFormat <$> #{peek struct OptionalFormat, has_value} ptr <*> #{peek struct OptionalFormat, value} ptr
    poke ptr (OptionalFormat has_value value) = 
        #{poke struct OptionalFormat, has_value} ptr has_value
            >> #{poke struct OptionalFormat, value} ptr value

data ImageInfo 
    = ImageInfo
    { width :: Word32 
    , height :: Word32 
    , rawFormat :: OptionalFormat 
    , color :: ColorType 
    , fileSize :: CSize 
    , hasAlpha :: CBool 
    , aspectRatio :: CDouble 
    } deriving Show

instance Storable ImageInfo where 
    sizeOf _ = #{size struct ImageInfo}
    alignment _ = #{alignment struct ImageInfo}
    peek ptr = do 
        width <- #{peek struct ImageInfo, width} ptr :: IO Word32
        height <- #{peek struct ImageInfo, height} ptr :: IO Word32
        format <- #{peek struct ImageInfo, format} ptr :: IO OptionalFormat
        color <- #{peek struct ImageInfo, color} ptr :: IO ColorType
        fileSize <- #{peek struct ImageInfo, file_size} ptr :: IO CSize
        hasAlpha <- #{peek struct ImageInfo, has_alpha} ptr :: IO CBool
        aspectRatio <- #{peek struct ImageInfo, aspect_ratio} ptr :: IO CDouble
        return $ ImageInfo width height format color fileSize hasAlpha aspectRatio
    poke ptr (ImageInfo width height format color fileSize hasAlpha aspectRatio) = do 
        #{poke struct ImageInfo, width} ptr width
        #{poke struct ImageInfo, height} ptr height
        #{poke struct ImageInfo, format} ptr format
        #{poke struct ImageInfo, color} ptr color
        #{poke struct ImageInfo, file_size} ptr fileSize
        #{poke struct ImageInfo, has_alpha} ptr hasAlpha
        #{poke struct ImageInfo, aspect_ratio} ptr aspectRatio
