{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

-- | A shallow embedding of the 'pic' language
module Pictures where

{------------------------------------------------------------------------------
- Domain-Specific Languages (DSLs)
-------------------------------------------------------------------------------

Examples:
- spreadsheets: Excel's macro language
- websites and styling: HTML, CSS
- interactive websites: Elm
- mathematics: Matlab, GNU Octave, Maple
- graphs: GraphViz
- relational databases: SQL
- textual analysis: regular expressions (grep, sed, ripgrep)
- build systems: Make, Shake
- embedded software: mbeddr (a dialect of C)
- packages/derivations: nix
- performance (math) libraries: Spiral
- parsers: yacc grammars



- focused on a "domain"/scope
    - uses domain-specific terms and concepts
- enable domain-specific...
    - abstractions (expressiveness),
    - analysis,
    - validation,
    - interpretation,
    - optimization,
    - tooling,
    - etc.

- executable? at times.
- might be...
    - interpreted,
    - compiled,
    - viewed,
    - embedded,
    - weaved with another language,
    - any mixture of the above
- more than just textual! they may be:
    - projectional,
    - diagrammatic,
    - form-based,
    - etc.


- Q: how can we define them?


Development Tools:
- Eclipse Epsilon (Xtext, etc.)
- JetBrains MPS
- Racket
- Spoofax

------------------------------------------------------------------------------}

-- | An abstract notion of picture.
--
-- We do not want to tie ourselves to a specific resolution when defining
-- pictures: this makes operations like rescaling much more annoying. Instead,
-- we shall think of an image as a morally consisting of an infinite grid of
-- pixels, arranged between (-1.0,-1.0) and (1.0, 1.0)
--
-- Colors are also represented as doubles ranging from 0.0 to 1.0. This is done
-- to make some of the math a bit easier, and to theoretically support variable
-- color depths.
type Pic = Double -> Double -> (Double, Double, Double)

-- * The 'pic' language

-- | Rotate a picture clockwise 90 degrees.
cw :: Pic -> Pic
cw pic x y = pic y (- x)

-- | Rotate a picture counter-clockwise 90 degrees.
ccw :: Pic -> Pic
ccw pic x y = pic (- y) x

-- | Flip a picture horizontally.
flipH :: Pic -> Pic
flipH pic x y = pic (- x) y

-- | Flip a picture vertically.
flipV :: Pic -> Pic
flipV pic x y = pic x (- y)

-- | Place two pictures next two each other.
beside :: Pic -> Pic -> Pic
beside left right x y | x >= 0 = right (2 * (x - 0.5)) y
                      | otherwise = left (2 * (x + 0.5)) y

-- | Place one picture above another.
above :: Pic -> Pic -> Pic                      
above top bot x y | y >= 0 = bot x (2 * (y - 0.5))
                  | otherwise = top x (2 * (y + 0.5))

-- | Make a picture that is just one color.
color :: Double -> Double -> Double -> Pic
color r g b _ _ = (r, g, b)

--------------------------------------------------------------------------------
-- 😎 Example Time 😎
--------------------------------------------------------------------------------

-- * Examples

-- | A solid black picture
black :: Pic
black = color 0 0 0

-- | A solid white picture
white :: Pic
white = color 1 1 1

-- | Tile together 4 Pics.
-- Order is @quad top-left top-right bottom-left bottom-right@
quad :: Pic -> Pic -> Pic -> Pic -> Pic
quad tl tr bl br = beside (above tl bl) (above tr br)

-- | Tile together 4 pics, rotating each time.
swirl :: Pic -> Pic
swirl p = quad p (cw p) (ccw p) (cw $ cw p)

checkerboard :: Pic
checkerboard = quad black white white black

cool :: Int -> Pic -> Pic
cool n p = swirl (leftCorner n p)
    where
      topEdge :: Int -> Pic -> Pic
      topEdge 0 p = p
      topEdge n p = above (beside (topEdge (n - 1) p) (topEdge (n - 1) p)) p

      leftEdge :: Int -> Pic -> Pic
      leftEdge 0 p = p
      leftEdge n p = beside (above (leftEdge (n - 1) p) (leftEdge (n - 1) p)) p
      
      leftCorner :: Int -> Pic -> Pic
      leftCorner 0 p = p
      leftCorner n p = quad (leftCorner (n - 1) p) (topEdge (n - 1) p) (leftEdge (n - 1) p) p

--------------------------------------------------------------------------------
-- ‼️ You do not need to understand the following code ‼️
-- This is used to render a 'Pic' to an image file, which
-- is not relevant to the content of this course :)
--------------------------------------------------------------------------------

-- * Rasterization
-- This is the process of transforming a 'Pic' into
-- a grid of RGB pixels.

-- | A raster consists of a grid of pixels.
-- The origin is in the upper-left hand corner.
type Raster = Int -> Int -> (Int, Int, Int)

-- | Convert a floating point color value between 0 and 1 to a discrete integer between 0 and 255.
toPixel :: Double -> Int
toPixel p = max 0 $ floor (p * 255)

-- | Convert a discrete integer between 0 and 255 to a floating point value between 0 and 1.
fromPixel :: Int -> Double
fromPixel p = max 0 (fromIntegral p / 255)

-- | Convert a floating point color value to an 8-bit RGB color.
toRGB :: (Double, Double, Double) -> (Int, Int, Int)
toRGB (r, g, b) = (toPixel r, toPixel g, toPixel b)

-- | Convert an 8-bit RGB color into a floating point color value.
fromRGB :: (Int, Int, Int) -> (Double, Double, Double)
fromRGB (r, g, b) = (fromPixel r, fromPixel g, fromPixel b)

-- | Transform a raster coordinate into a floating point value between -1 and 1.
fromDim :: Int -> Int -> Double
fromDim dim x = (2/fromIntegral dim * fromIntegral x) - 1.0

-- | Transform a floating point value between -1 and 1 into a raster coordinate.
toDim :: Int -> Double -> Int
toDim dim x = floor $ (x + 1) * fromIntegral dim / 2

-- | Rasterize a picture into a width * height grid of pixels.
rasterize :: Int -> Int -> Pic -> Raster
rasterize width height pic x y =
    toRGB (pic (fromDim width x) (fromDim height y))

-- | Use a rasterized image as the basis of a picture.
sample :: Int -> Int -> Raster -> Pic    
sample width height pic x y =
    fromRGB (pic (toDim width x) (toDim height y))

-- * Encoding
-- We are using the PPM image format, which is extremely simple.
--
-- A PPM image consists of a header, followed by RGB values specified in ASCII, separated by newlines.
-- The header format is:
--
-- P3
-- <width> <height> <depth>
--
-- where <width> and <height> give the width and height of the image in pixels, and <depth>
-- is the color depth. For our purposes, we will use 255, which corresponds to an 8-bit color depth.
--
-- See https://netpbm.sourceforge.net/doc/ppm.html the official spec.

-- | Create a PPM image header, using an 8-bit color depth.
encodeHeader :: Int -> Int -> String
encodeHeader width height =
    "P3\n" <> show width <> " " <> show height <> " 255\n"

-- | Encode a single 'Color' as a PPM pixel.
encodeColor :: (Int, Int, Int) -> String    
encodeColor (r, g, b) = show r <> " " <> show g <> " " <> show b <> "\n"

-- | Encode the pixels of a 'Pic' as PPM pixels.
-- | Note that the picture is centered at (0,0)
encodePixels :: Int -> Int -> Raster -> String
encodePixels width height pic = do
    y <- [0..height - 1]
    x <- [0..width - 1]
    encodeColor (pic x y)

-- | Encode a raster as a PPM file.
encodeRaster :: Int -> Int -> Raster -> String
encodeRaster width height pic =
    encodeHeader width height <> encodePixels width height pic

-- | Save a pic to an image file.
draw :: FilePath -> Int -> Int -> Pic -> IO ()
draw path width height pic = writeFile path (encodeRaster width height (rasterize width height pic))

{- Recap: Shallow Languages

- constrained by host language's type system,
- defined as a specific _interpretation_ about the abstract syntax of our DSLs,
- extensible in terms,
- not extensible in interpretation variabilities (no reuse of an AST),

-}
