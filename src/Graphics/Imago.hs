{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}

module Graphics.Imago
    ( TransformM
    , Degree(..)
    , Direction(..)
    , Filter(..)
    , Format(..)
    , resize
    , rotate
    , flip
    , grayscale
    , blur
    , brighten
    , contrast
    , runFileTransform
    , runBufferTransform
    ) where

import           Control.Monad.Free    (Free (..), MonadFree, liftF)
import           Control.Monad.Free.TH
import           Data.Binary           (Word32)
import           Data.ByteString       (ByteString)
import           Foreign               (fromBool)
import           Graphics.LibImago
import           Prelude               hiding (flip)

data Transform a
    = Resize
        { w          :: Word32
        , h          :: Word32
        , filterType :: Filter
        , exact      :: Bool
        , next       :: a
        }
    | Rotate Degree a
    | Flip Direction a
    | Grayscale a
    | Blur Float a
    | Brighten Int a
    | Contrast Float a
    deriving (Show, Eq, Ord, Functor)

makeFree ''Transform

type TransformM = Free Transform

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


runFileTransform :: FilePath -> Format -> TransformM () -> IO (Either ImagoStatus ByteString)
runFileTransform path fmt transform =
    rawProcessImage path (unfold transform) fmt

runBufferTransform :: ByteString -> Maybe Format -> Format -> TransformM () -> IO (Either ImagoStatus ByteString)
runBufferTransform contents inputFormat outputFormat transform =
    rawProcessBuffer contents inputFormat (unfold transform) outputFormat
