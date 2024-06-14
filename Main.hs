-- | Module: Main

module Main where

import Control.Monad (foldM_, when)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Data.Maybe (fromMaybe, listToMaybe, isNothing)
import Data.Either (partitionEithers)

import Options (parseArgs, Options(..))
import Parser (parse, Expr(..), Symbol(..))
import Geometry (Transformation, scaleMatrix, moveMatrix, rotateMatrix, Transformable(..))
import Stack (peek, pop, push, modHead)
import Color (Color, readColor, RGB(..))
import Display (newScreen, clearImage, save, display, animate, asAscii, Image)
import Draw (line, circle, hermite, cbezier, qbezier, box, sphere, torus)
import Lighting (Ambient(..), Material(..), Lights, PointLight(..))
import Knob (parseKnob)
import Vector (Vec3(..))


-- | run fabricater
main :: IO ()
main = do
  args <- parseArgs <$> getArgs
  (frames, basename, syms, exprs) <- parse <$> maybe getContents readFile (getScript args)
  let animated = frames /= 1
  when (animated && isNothing basename) $ hPutStrLn stderr "Warning: defaulting basename to 'img'."

  when (not $ silent args) $
    maybe putStr writeFile (getOutput args) $ unlines $
    ["Frames: " ++ show frames, "Basename: " ++ fromMaybe "N/A" basename]
    ++ ["", "------", ""]
    ++ map show syms
    ++ ["", "------", ""]
    ++ map show exprs
  
  when (not (silent args) && willRun args) $ putStrLn "------"

  when (willRun args) $ do
    img <- newScreen 500 500
    mapM_ (\f -> putStrLn ("Frame " ++ show f ++ " / " ++ show (frames - 1)) >> run (getDisp args) img (readColor 8 62 100) (readColor 252 252 252) animated basename syms exprs f) [0 .. frames - 1]
    when animated $ animate 30 (fromMaybe "img" basename) (getDisp args)

-- | Takes an optional filepath for `display`, an image to draw to, the background and foreground colors, and finally a list of Exprs
run :: Maybe FilePath -> Image -> Color -> Color -> Bool -> Maybe String -> [Symbol] -> [Expr] -> Integer -> IO ()
run dispMode i bgcol fgcol animated basename syms exprs frame = do
  clearImage i bgcol
  foldM_ eval [] exprs
  when animated $ save asAscii ("img/" ++ fromMaybe "img" basename ++ "_" ++ take (3 - length (show frame)) (repeat '0') ++ show frame) i
  where
    lights = getLights syms frame
    eval :: [Transformation] -> Expr -> IO [Transformation]
    eval xs = \case
      Line p0 p1          -> render Nothing $ line p0 p1 
      Circle p r          -> render Nothing $ circle p r 
      Hermite p0 p1 r0 r1 -> render Nothing $ hermite p0 p1 r0 r1 
      CBezier p0 p1 p2 p3 -> render Nothing $ cbezier p0 p1 p2 p3 
      QBezier p0 p1 p2    -> render Nothing $ qbezier p0 p1 p2 
      Box    m p w h d    -> render m $ box p w h d 
      Sphere m c r        -> render m $ sphere c r 
      Torus  m c r1 r2    -> render m $ torus c r1 r2
      Display             -> when (not animated) (maybe display (save asAscii) dispMode i) >> return xs
      Save fname          -> when (not animated) (save asAscii fname i) >> return xs
      Clear               -> clearImage i bgcol >> return xs
      Push                -> return $ push xs
      Pop                 -> return $ pop xs
      Scale k sx sy sz      -> return $ modHead xs $ scaleMatrix (knob' k sx) (knob' k sy) (knob' k sz)
      Move  k tx ty tz      -> return $ modHead xs $ moveMatrix (knob' k tx) (knob' k ty) (knob' k tz)
      Rotate k axis theta   -> return $ modHead xs $ rotateMatrix axis (knob' k theta)
      where
        render :: (Transformable a) => Maybe String -> [a] -> IO [Transformation]
        render mat = (>> return xs) . draw i lights (matlookup syms mat) fgcol . map (apply (peek xs))
        knob' key x = knob syms frame key x

headDef :: a -> [a] -> a
headDef def = fromMaybe def . listToMaybe

getLights :: [Symbol] -> Integer -> Lights
getLights syms frame = (ambient, map (resolvePoints syms frame) points)
  where ambient = headDef basic [x | AmbientVar x <- syms]
        points = [x | LightVar x <- syms]
        basic = Ambient (RGB 0 0 0)

resolvePoints :: [Symbol] -> Integer -> PointLight -> PointLight
resolvePoints syms frame (PointLight (Vec3 x y z) plcolor (x1,y1,z1)) = PointLight (Vec3 (knob syms frame x1 x) (knob syms frame y1 y) (knob syms frame z1 z)) plcolor (Nothing, Nothing, Nothing)

matlookup :: [Symbol] -> Maybe String -> Material
matlookup ls = maybe basic $ \key -> headDef basic [x | MaterialVar key' x <- ls, key == key']
  where basic = Material (RGB 0.2 0.2 0.2) (RGB 0.2 0.2 0.2) (RGB 0.2 0.2 0.2)

knob :: [Symbol] -> Integer -> Maybe String -> Double -> Double
knob _ _ Nothing total = total
knob syms frame (Just key) total = myFirst $ map (($ frame) . parseKnob) defs
  where
    defs = [x | KnobVar key' x <- syms, key == key']
    myFirst ls = headDef total $ uncurry (flip (++)) $ partitionEithers ls
