# Imago

## Why

Imago is a light wrapper around Rust's `image` crate, providing a simple monadic interface to its well-optimized, well-maintained image processing algorithms. 

Building [Texel](https://faycarsons.xyz), I found myself needing an optimized image processing library capable of basic optimization, not wanting to shell out money to cloud services like Akamai or add a Rust microservice and all the complexity that comes with that, I decided to build my own. 

## How to use it

Image transformations are expressed as a free monad, `Program`:

```haskell
import Graphics.Imago

myImageTransform :: (Int, Int) -> Program
myImageTransform (width, height) = do 
    resize width height Nearest False
    quality 80
    grayscale
    convert WebP

resizeImage :: FilePath -> IO ByteString
resizeImage path = do 
  info <- getFileInfo path
  let ratio = round . (* 0.8) . fromIntegral
      targetDimensions = both ratio ratio $ dimensions info
      imageProcessingProgram = myImageTransform targetDimensions
  runFileTransform path Png imageProcessingProgram
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
