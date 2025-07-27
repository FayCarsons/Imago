# Imago

## Why

Imago is an image processing library focused on the needs of Backend services.

Working on my app [Texel](https://faycarsons.xyz) I found myself needing image processing but was unhappy with the libraries in the Haskell ecosystem, which are unmaintained or didn't meet my performance requirements. I wanted something that was simple and lightning fast without having to shell out money to cloud providers like AWS or Akamai. 

Image processing is entirely handled in Rust, giving us excellent performance *and* a nice monadic interface.

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