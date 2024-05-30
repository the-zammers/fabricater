-- | Module: Main

module Main where

import Control.Monad (foldM_, when)
import System.Environment (getArgs)

import Options (parseArgs, Options(..))
import Parser (parse, Expr(..), Symbol(..))
import Geometry (Transformation, scaleMatrix, moveMatrix, rotateMatrix, Transformable(..))
import Stack (peek, pop, push, modHead)
import Color (Color, readColor, RGB(..))
import Display (newScreen, clearImage, save, display, asAscii, Image)
import Draw (line, circle, hermite, cbezier, qbezier, box, sphere, torus)
import Lighting (Material(..))


-- | run fabricater
main :: IO ()
main = do
  args <- parseArgs <$> getArgs
  exprs <- parse <$> maybe getContents readFile (getScript args)

  when (not $ silent args) $
    maybe putStr writeFile (getOutput args) $ unlines . map show $ snd exprs

  when (willRun args) $ do
    img <- newScreen 500 500
    uncurry (run (getDisp args) img (readColor 8 62 100) (readColor 252 252 252)) $ exprs

-- | Takes an optional filepath for `display`, an image to draw to, the background and foreground colors, and finally a list of Exprs
run :: (Maybe FilePath) -> Image -> Color -> Color -> [Symbol] -> [Expr] -> IO ()
run dispMode i bgcol fgcol syms exprs = clearImage i bgcol >> foldM_ eval [] exprs
  where
    eval :: [Transformation] -> Expr -> IO [Transformation]
    eval xs = \case
      Line p0 p1          -> render "" $ line p0 p1 
      Circle p r          -> render "" $ circle p r 
      Hermite p0 p1 r0 r1 -> render "" $ hermite p0 p1 r0 r1 
      CBezier p0 p1 p2 p3 -> render "" $ cbezier p0 p1 p2 p3 
      QBezier p0 p1 p2    -> render "" $ qbezier p0 p1 p2 
      Box    m p w h d    -> render m $ box p w h d 
      Sphere m c r        -> render m $ sphere c r 
      Torus  m c r1 r2    -> render m $ torus c r1 r2
      Display             -> maybe display (save asAscii) dispMode i >> return xs
      Save fname          -> save asAscii fname i >> return xs
      Clear               -> clearImage i bgcol >> return xs
      Push                -> return $ push xs
      Pop                 -> return $ pop xs
      Scale sx sy sz      -> return $ modHead xs $ scaleMatrix sx sy sz
      Move  tx ty tz      -> return $ modHead xs $ moveMatrix tx ty tz
      Rotate axis theta   -> return $ modHead xs $ rotateMatrix axis theta
      where
        render :: (Transformable a) => String -> [a] -> IO [Transformation]
        render mat = (>> return xs) . draw i (symlookup mat syms) fgcol . map (apply (peek xs))

symlookup :: String -> [Symbol] -> Material
symlookup key' ls = maybe basic id $ go key' ls
  where go _key [] =  Nothing
        go  key (Symbol x y:xys)
          | key == x  =  Just y
          | otherwise =  go key xys
        basic = Material (RGB 0.2 0.2 0.2) (RGB 0.9 0.6 0.6) (RGB 0.2 0.4 0.4)
