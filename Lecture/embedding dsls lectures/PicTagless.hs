{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module PicTagless where

{-------------------------------------------------------------------------------
-- Finally Tagless Encodings

Recall the Pros and Cons of Shallow and Deep embeddings:

Shallow:
✅ Easy to extend
✅ Nice syntax
❌ Hard to re-interpret
❌ Cannot easily perform program transformations

Deep:
❌ Hard to extend
❌ Ugly syntax
✅ Easy to reinterpret
✅ Can perform program transformations

Can we find an encoding that is all ✅ and no ❌?

Enter: Finally Tagless!

💡Big Idea: define the domain specific langauge as an abstract interface that we can program against!

Let's see how we fare when it comes to ✅
-}

-- | Here, we are representing the Pic language via an abstract interface.
class Pic p where
    cw :: p -> p
    ccw :: p -> p
    flipH :: p -> p
    flipV :: p -> p
    beside :: p -> p -> p
    above :: p -> p -> p
    black :: p
    white :: p

-- This code should look familiar: it's the same we had from before!    
-- Nice pretty syntax: no building abstract syntax trees.
quad :: Pic p => p -> p -> p -> p -> p
quad tl tr bl br = above (beside tl tr) (beside bl br)

checkerboard :: Pic p => p
checkerboard = quad black white white black

swirl :: Pic p => p -> p
swirl p = quad p (cw p) (ccw p) (cw $ cw p)

topEdge :: Pic p => Int -> p -> p
topEdge 0 p = p
topEdge n p =
    above
     (beside (topEdge (n - 1) p) (topEdge (n - 1) p))
     p

leftEdge :: Pic p => Int -> p -> p
leftEdge 0 p = p
leftEdge n p =
    beside (above (leftEdge (n - 1) p) (leftEdge (n - 1) p))
    p
    
leftCorner :: Pic p => Int -> p -> p
leftCorner 0 p = p
leftCorner n p =
    quad
      (leftCorner (n - 1) p)
      (topEdge (n - 1) p)
      (leftEdge (n - 1) p)
      p

fractal :: Pic p => Int -> p -> p
fractal n p = swirl (leftCorner n p)

--------------------------------------------------------------------------------
-- An Interpretation of Pic: Emoji Pics

type EmojiPic = Double -> Double -> Char

instance Pic EmojiPic where
    cw pic x y = pic y (- x)
    ccw pic x y = pic (- y) x
    flipH pic x y = pic (- x) y
    flipV pic x y = pic x (- y)
    beside left right x y | x >= 0 = right (2 * (x - 0.5)) y
                          | otherwise = left (2 * (x + 0.5)) y
    above top bot x y | y >= 0 = bot x (2 * (y - 0.5))
                      | otherwise = top x (2 * (y + 0.5))
    black _ _ = '⬛'
    white _ _ = '⬜'

-- | Look at how easy it is to extend the language!
class Pic p => TextPic p where
    glyph :: Char -> p

instance TextPic EmojiPic where
    glyph c _ _ = c

kindaCool :: Pic p => p    
kindaCool = fractal 3 checkerboard

cool :: TextPic p => p    
cool = fractal 3 (quad (glyph '💯') (glyph '🤠') (glyph '🤠') (glyph '💯'))
    
--------------------------------------------------------------------------------
-- Initial Encodings

-- | Recall the deep embedding from before: this meets the pic interface!
data PicExpr
  = Cw PicExpr
  | Ccw PicExpr
  | FlipH PicExpr
  | FlipV PicExpr
  | Beside PicExpr PicExpr
  | Above PicExpr PicExpr
  | Black
  | White
  deriving Show

instance Pic PicExpr where
    cw = Cw
    ccw = Ccw
    flipH = FlipH
    flipV = FlipV
    beside = Beside
    above = Above
    black = Black
    white = White

-- | Furthermore, every syntactic representation of a pic program can be interpreted
-- into an arbitrary instance of the 'Pic' interface.
interpret :: (Pic p) => PicExpr -> p
interpret (Cw p) = cw $ interpret p
interpret (Ccw p) = ccw $ interpret p
interpret (FlipH p) = flipH $ interpret p
interpret (FlipV p) = flipV $ interpret p
interpret (Beside l r) = beside (interpret l) (interpret r)
interpret (Above t b) = above (interpret t) (interpret b)
interpret Black = black
interpret White = white

-- | One nice thing about syntax is we can transform it!
optimize :: PicExpr -> PicExpr
optimize (Cw p) =
    case optimize p of
      (Ccw p) -> p
      (Cw (Cw (Cw p))) -> p
      p -> Cw p
optimize (Ccw p) =
    case optimize p of
      (Cw p) -> p
      (Ccw (Ccw (Ccw p))) -> p
      p -> Ccw p
optimize (FlipH p) =
    case optimize p of
      (FlipH p) -> p
      p -> FlipH p
optimize (FlipV p) =
    case optimize p of
      (FlipV p) -> p
      p -> FlipV p
optimize (Beside l r) = Beside (optimize l) (optimize r) 
optimize (Above l r) = Above (optimize l) (optimize r) 
optimize Black = Black
optimize White = White

-- | An inefficient program.
badProg :: Pic p => p
badProg =
    cw $ cw $ ccw $ cw $ cw $ cw $
    cw $ cw $ ccw $ cw $ cw $ cw $
    cw $ cw $ ccw $ cw $ cw $ cw $
    cw $ cw $ ccw $ cw $ cw $ cw $
    cw $ cw $ ccw $ cw $ cw $ cw $
    cw $ cw $ ccw $ cw $ cw $ cw $
    cw $ cw $ ccw $ cw $ cw $ cw $
    cw $ cw $ ccw $ cw $ cw $ cw $
    checkerboard

--------------------------------------------------------------------------------
-- ‼️ You do not need to understand the following code ‼️
-- This is used to render an 'EmojiPic' as a bunch of emojis,
-- is not relevant to the content of this course :)
--------------------------------------------------------------------------------
    
fromDim :: Int -> Int -> Double
fromDim dim x = (2/fromIntegral dim * fromIntegral x) + 1/fromIntegral dim - 1.0

renderEmoji :: Int -> Int -> EmojiPic -> String
renderEmoji width height pic = drawLine =<< [0..height - 1]
  where
    drawLine :: Int -> String
    drawLine y = map (\x -> pic (fromDim width x) (fromDim height y)) [0..width - 1] ++ "\n"

drawEmoji :: Int -> Int -> EmojiPic -> IO ()
drawEmoji width height pic = putStrLn (renderEmoji width height pic)
