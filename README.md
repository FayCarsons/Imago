# Imago

## Why

Imago is a light wrapper around Rust's `image` crate, providing a simple monadic interface to its well-optimized, well-maintained image processing algorithms. 

Building [Texel](https://faycarsons.xyz) I found myself needing an image processing library capable of basic optimization, not wanting to shell out money to cloud services like Akamai or add a Rust microservice and all the complexity and overhead that comes with that, I decided to build my own. 

## How do I use it

Image transformations can be defined in `do` blocks: 

```haskell
import qualified Graphics.Imago as Imago
import Graphics.Imago (runFileTransform, resize, grayscale, Filter(..), Format(..))

resizePlusGrayscale :: Int -> Int -> TransformM ()
resizePlusGrayscale width height = do 
    resize width height Linear False
    grayscale

resizeImage :: FilePath -> IO ByteString
resizeImage path = runFileTransform path Png resizePlusGrayscale
```

Current features include:

- Resize, rotate, flip, blur, etc
- Format conversion
- Composable image transformations
- Can process either via a path or a flat image buffer (`runBufferTransform` is provided and takes an optional input format if you suspect it will be difficult to infer)

Future plans, in no particular order:

- Image composition 
- Cropping
- Fetching image data

# Installation

As the package is not yet on Hackage you will have to manually clone the library and require it from your program.