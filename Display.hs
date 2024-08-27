-- | Module: Display

module Display where

import qualified Data.Array.IO as AI
import qualified Data.Array.MArray as AM
import qualified System.Process as P
import System.IO (hPutStrLn, hClose)
import Control.Monad (when, void)
import Color (Color, maxValue)

-- | Image type
type Image = AI.IOArray Pos (Color, Double)

-- | Position in image--should not be used elsewhere
type Pos = (Int, Int)

-- | Creates a new Image object to write into
newScreen :: Int -> Int -> IO Image
newScreen width height = AM.newArray_ bounds
  where bounds = ((0,0), (height-1, width-1))

-- | Fills entire image with given color
clearImage :: Image -> Color -> IO ()
clearImage image defaultColor = do
  ((miny, minx), (maxy, maxx)) <- AM.getBounds image
  mapM_ (flip (AM.writeArray image) (defaultColor, negInf)) [(y,x) | y <- [miny..maxy], x <- [minx..maxx]]
  where negInf = encodeFloat (-1) $ snd $ floatRange (0 :: Double)

-- | Save an Image to the given file using the given format
save :: (Image -> IO String) -> FilePath -> Image -> IO ()
--save convert fname image = writeFile fname =<< convert image
save convert fname image = do
  (Just hin, _, _, _) <- P.createProcess_ "fabricater" (P.shell $ "convert - " ++ fname) {P.std_in = P.CreatePipe}
  hPutStrLn hin =<< convert image
  hClose hin

-- | Display an image using ImageMagick's `display` or `convert`
display :: Image -> IO ()
display image = do
  (Just hin, _, _, _) <- P.createProcess_ "fabricater" (P.shell $ "display -") {P.std_in = P.CreatePipe}
  hPutStrLn hin =<< asAscii image
  hClose hin

animate :: Double -> String -> Maybe FilePath -> IO ()
animate fps basename fname = void $ P.createProcess_ "fabricater" (P.shell $ maybe "animate" (const "convert") fname ++ " -delay " ++ show (100 / fps) ++ " img/" ++ basename ++ "_* " ++ maybe "" (' ':) fname)

clearcache :: String -> IO ()
clearcache basename = void $ P.createProcess_ "fabricater" (P.shell $ "mkdir -p img/ && rm -f img/" ++ basename ++ "_*")

-- | Generate the ASCII PPM representation of the image
asAscii :: Image -> IO String
asAscii screen = do
  ((miny, minx), (maxy, maxx)) <- AM.getBounds screen
  pixels <- AM.getElems screen
  return $  "P3 " ++ show (maxx-minx+1) ++ " " ++ show (maxy-miny+1) ++ " " ++ show maxValue ++ "\n" ++ unlines (map (show . fst) $ pixels)

-- | Draw a line with endpoints p0 and p1
draw_line :: Image -> Color -> (Pos, Double) -> (Pos, Double) -> IO ()
draw_line image color (p0,z0) (p1,_) = bresenham (\p -> plot image color (p,z0)) p0 p1

-- | Implementation of Bresenham's line algorithm:
bresenham :: (Pos -> IO ()) -> Pos -> Pos -> IO ()
bresenham plot' p0@(x0,y0) p1@(x1,y1)
  | x0 > x1 = bresenham plot' p1 p0
  | abs b >= abs a && 0 <= a = octant1 p0 (a + b `div` 2)
  | abs a >= abs b && 0 <= a = octant2 p0 (a `div` 2 + b)
  | abs b >= abs a && 0 >= a = octant8 p0 (a - b `div` 2)
  | abs a >= abs b && 0 >= a = octant7 p0 (a `div` 2 - b)
  | otherwise = error "unknown case in Bresenham's line algorithm"
  where
    a =  2 * (y1 - y0)
    b = -2 * (x1 - x0)
    octant1 p@( x, _y) d
      | x<=x1 && d>0 = plot' p >> octant1 (addPos (1, 1) p) ((+) (a+b) d)
      | x<=x1        = plot' p >> octant1 (addPos (1, 0) p) ((+) (a  ) d)
      | otherwise = pure ()
    octant2 p@(_x,  y) d
      | y<=y1 && d<0 = plot' p >> octant2 (addPos (1, 1) p) ((+) (a+b) d)
      | y<=y1        = plot' p >> octant2 (addPos (0, 1) p) ((+) (  b) d)
      | otherwise = pure ()
    octant8 p@( x, _y) d
      | x<=x1 && d<0 = plot' p >> octant8 (addPos (1,-1) p) ((+) (a-b) d)
      | x<=x1        = plot' p >> octant8 (addPos (1, 0) p) ((+) (a  ) d)
      | otherwise = pure ()
    octant7 p@(_x,  y) d
      | y>=y1 && d>0 = plot' p >> octant7 (addPos (1,-1) p) ((+) (a-b) d)
      | y>=y1        = plot' p >> octant7 (addPos (0,-1) p) ((+) ( -b) d)
      | otherwise = pure ()
    addPos (f0,s0) (f1,s1) = (f0+f1, s0+s1)

scanline :: Image -> Color -> Int -> Double -> Double -> Double -> Double -> IO ()
scanline img col y x0 x1 z0 z1
  | x0 > x1 = scanline img col y x1 x0 z1 z0
  | otherwise = sequence_ [plot img col ((floor x,y), z0 + (z1 - z0) / (floor' x1 - floor' x0) * (x - x0)) | x <- take (floor x1 - floor x0) [x0 .. ]]
  where floor' = fromIntegral . (floor :: Double -> Integer)

-- | Plots a single point on the screen at a given position in a given color
plot :: Image -> Color -> (Pos, Double) -> IO ()
plot screen color ((x,y),z) = do
  ((miny,minx), (maxy,maxx)) <- AM.getBounds screen
  when (minx <= x && x <= maxx && miny <= y && y <= maxy) $ do
    (_, z') <- AM.readArray screen (maxy-y, x)
    when (z' < z) $ AM.writeArray screen (maxy-y, x) (color, z)
