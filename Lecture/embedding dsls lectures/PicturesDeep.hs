{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wall #-}
{-# HLINT ignore "Eta reduce" #-}

-- | A deep embedding of the 'pic' language
module PicturesDeep where

import Pictures (draw, Pic)

-- Expression problem -- Philip Wadler

data PicDesc where
    CW :: PicDesc -> PicDesc
    CCW :: PicDesc -> PicDesc
    FlipH :: PicDesc -> PicDesc
    FlipV :: PicDesc -> PicDesc
    Beside :: PicDesc -> PicDesc -> PicDesc
    Above :: PicDesc -> PicDesc -> PicDesc
    Color :: (Double, Double, Double) -> PicDesc
    Diagonal :: PicDesc -> PicDesc -> PicDesc

color :: Double -> Double -> Double -> (Double, Double, Double)
color r g b = (r, g, b)

renderDesc :: PicDesc -> Pic
renderDesc (Color rgb) _ _ = rgb
renderDesc (CW pd) x y    = renderDesc pd y (- x)
renderDesc (CCW pd) x y   = renderDesc pd (- y) x
renderDesc (FlipH pd) x y = renderDesc pd (- x) y
renderDesc (FlipV pd) x y = renderDesc pd x (- y)
renderDesc (Beside l r) x y
    | x >= 0 = renderDesc r (2 * (x - 0.5)) y
    | otherwise = renderDesc l (2 * (x + 0.5)) y
renderDesc (Above t b) x y
    | y >= 0 = renderDesc b x (2 * (y - 0.5))
    | otherwise = renderDesc t x (2 * (y + 0.5))
renderDesc (Diagonal t b) x y
    | - y >= x  = renderDesc t x y
    | otherwise = renderDesc b x y

shade :: Double -> PicDesc -> PicDesc
shade fac (CW pd) = CW $ shade fac pd
shade fac (CCW pd) = CCW $ shade fac pd
shade fac (FlipH pd) = CCW $ shade fac pd
shade fac (FlipV pd) = CCW $ shade fac pd
shade fac (Beside l r) = Beside (shade fac l) (shade fac r)
shade fac (Above t b) =  Above (shade fac t) (shade fac b)
shade fac (Color (r, g, b)) = Color (shadeCol r, shadeCol g, shadeCol b)
    where shadeCol col = col * (1 - fac)
shade fac (Diagonal t b) =  Diagonal (shade fac t) (shade fac b)

--------------------------------------------------------------------------------
-- 😎 Example Time 😎
--------------------------------------------------------------------------------

-- * Examples

-- | A solid black picture
black :: PicDesc
black = Color $ color 0 0 0

-- | A solid white picture
white :: PicDesc
white = Color $ color 1 1 1

somegrey :: PicDesc
somegrey = shade 0.33 white

somegreysomewhite :: PicDesc
somegreysomewhite = Diagonal white somegrey

-- | Tile together 4 Pics.
-- Order is @quad top-left top-right bottom-left bottom-right@
quad :: PicDesc -> PicDesc -> PicDesc -> PicDesc -> PicDesc
quad tl tr bl br = Beside (Above tl bl) (Above tr br)

-- | Tile together 4 pics, rotating each time.
swirl :: PicDesc -> PicDesc
swirl p = quad p (CW p) (CCW p) (CW $ CW p)

checkerboard :: PicDesc
checkerboard = quad black white white black

--------------------------------------------------------------------------------
-- ‼️ You do not need to understand the following code ‼️
-- This is used to render a 'Pic' to an image file, which
-- is not relevant to the content of this course :)
--------------------------------------------------------------------------------

drawDesc :: FilePath -> Int -> Int -> PicDesc -> IO ()
drawDesc path width height picDesc = draw path width height (renderDesc picDesc)

{-

Shallow embedding:

    * piggyback on host language's type system

    Pros:
        * easy extensibility terms

    Cons:
        * no reuse of ASTs, things are immediately evaluated, no reuse!


Deep embedding:

    Pros:
        * easy reuse of programs written in the DSL
        * custom validity system
        * simulatable
        * domain-specific interpretations:
            * tool creation
            * analysis
            * re-writing
            * optimization passes
            * etc.

    Cons:
        * extra tag
        * term set is typically not 'open', we can't easily extend a language
          with recompilation and defining interpretation of new terms for all 
          previously defined interpretors
            * (expression problem)


abstract class Expression {
    public Val evaluate();
}

class BinaryAdd extending Expression {...}



-}
