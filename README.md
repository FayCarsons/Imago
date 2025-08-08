# Imago

## Why

Imago is a light Haskell wrapper around Rust's `image` crate, providing a simple monadic interface to its well-optimized, well-maintained image processing utilities, built with enterprise needs in mind.

Building [Texel](https://faycarsons.xyz), I found myself needing an optimized image processing library capable of basic image optimization, not wanting to shell out money on cloud services like Akamai, or add a Rust microservice and all the complexity that comes with that, I decided to build my own. 

## How to use it

Image transformations are expressed as the `Program` monad:

```haskell
import Graphics.Imago

resizeAndConvert :: (Int, Int) -> Program
resizeAndConvert (width, height) = do 
    resize width height Nearest False
    convert WebP

reduceQuality :: Program
reduceQuality = quality 80
```

You can them run them on files or flat buffers:

```haskell
resizeImage :: FilePath -> IO ByteString
resizeImage path = do 
  info <- getFileInfo path
  let (width, height) = dimensions info
      targetDimensions = (ratio width, ratio height)
      imageProcessingProgram =
        resizeAndConvert targetDimensions >>= reduceQuality
  runFileTransform path Png imageProcessingProgram
  where ratio = round . (* 0.8) . fromIntegral
```

Current features include:

- Resize, rotate, flip, blur, quality etc
- Format conversion
- Composable image transformations

Future plans, in no particular order:

- Image composition 
- Cropping
- Haskell-side optimization

# Installation

As the package is not yet on Hackage you will have to manually `git clone` the library and require it from your program via a cabal.project file:

`git clone https://github.com/FayCarsons/Imago`

Then put something like this in your `cabal.project` file:
```
packages:
  .
  path/to/Imago
```
