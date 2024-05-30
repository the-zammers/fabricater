{-# LANGUAGE DataKinds, FlexibleInstances #-} 

-- | Module: Geometry

module Geometry where

import Matrix (Matrix, listToMatrix, matrixToList, matrixMult)
import Control.Arrow ((***))
import Control.Monad (when)
import Data.List (sortBy, zipWith5)
import Data.Function (on)

import Display (draw_line, scanline, Image, Pos)
import Color (Color)
import Vector (isFrontFace)
import Lighting (getLighting, Material)

-- | Point
type Point = (Double, Double, Double)
getX, getY, getZ :: Point -> Double
getX (x,_,_) = x
getY (_,y,_) = y
getZ (_,_,z) = z

-- | List of edges
type EdgeList = [Edge]
-- | Single edge
type Edge = Matrix 4 2 Double

-- | List of triangles
type TriList = [Triangle]
-- | Single triangle
type Triangle = Matrix 4 3 Double

-- | Tranformation stack
type TStack = [Transformation]
-- | Transformation
type Transformation = Matrix 4 4 Double

-- | Axis of rotation
data Axis = X | Y | Z deriving (Eq, Read, Show)

class Transformable a where
  apply :: Transformation -> a -> a
  draw :: Image -> Material -> Color -> [a] -> IO ()

-- | Turns two Points into an Edge
mkEdge :: Point -> Point -> Edge
mkEdge (x0,y0,z0) (x1,y1,z1)
  = listToMatrix [x0,x1,
                  y0,y1,
                  z0,z1,
                   1, 1]

-- | Turns an Edge into two Points
unEdge :: Edge -> (Point, Point)
unEdge x = ((y!!0, y!!2, y!!4), (y!!1, y!!3, y!!5))
  where y = matrixToList x

-- | Turns three Points into a Triangle
mkTri :: Point -> Point -> Point -> Triangle
mkTri (x0,y0,z0) (x1,y1,z1) (x2,y2,z2)
  = listToMatrix [x0,x1,x2,
                  y0,y1,y2,
                  z0,z1,z2,
                   1, 1, 1]

-- | Turns a Triangle into three Points
unTri :: Triangle -> (Point, Point, Point)
unTri x = ((y!!0, y!!3, y!!6), (y!!1, y!!4, y!!7), (y!!2, y!!5, y!!8))
  where y = matrixToList x

-- | Converts a Point to a Pos and z-buffer Double
toPos :: Point -> (Pos, Double)
toPos (x,y,z) = ((round x, round y), z)

-- | Creates a dilation matrix given three axes of scale
scaleMatrix :: Double -> Double -> Double -> Transformation
scaleMatrix sx sy sz
  = listToMatrix [sx,  0,  0,  0,
                   0, sy,  0,  0,
                   0,  0, sz,  0,
                   0,  0,  0,  1]

-- | Creates a translation matrix given three axes of movement
moveMatrix :: Double -> Double -> Double -> Transformation
moveMatrix tx ty tz
  = listToMatrix [ 1,  0,  0, tx,
                   0,  1,  0, ty,
                   0,  0,  1, tz,
                   0,  0,  0,  1]

-- | Creates a rotation matrix given an axis and a degree of rotation
rotateMatrix :: Axis -> Double -> Transformation
rotateMatrix axis theta = case axis of
  Z -> listToMatrix [ c, -s,  0,  0,
                      s,  c,  0,  0,
                      0,  0,  1,  0,
                      0,  0,  0,  1]

  X -> listToMatrix [ 1,  0,  0,  0,
                      0,  c, -s,  0,
                      0,  s,  c,  0,
                      0,  0,  0,  1]

  Y -> listToMatrix [ c,  0,  s,  0,
                      0,  1,  0,  0,
                     -s,  0,  c,  0,
                      0,  0,  0,  1]
  where theta' = theta * pi / 180
        c      = cos theta'
        s      = sin theta'

-- | Draws an EdgeList to the screen
instance Transformable Edge where
  apply = matrixMult
  draw img _ col = mapM_ ( uncurry (draw_line img col)
                       . (toPos *** toPos)
                       . unEdge
                       )

-- | Draws a TriList to the screen
instance Transformable Triangle where
  apply = matrixMult
  draw img mat _ = mapM_ ( (\tri -> when (isFrontFace tri) (drawTri tri))
                       . unTri
                       )
    where drawTri tri = fillTri img (getLighting mat tri) tri
                                -- >> draw' (toPos p0) (toPos p1)
                                -- >> draw' (toPos p1) (toPos p2)
                                -- >> draw' (toPos p2) (toPos p0)
          --draw' = draw_line img col

fillTri :: Image -> Color -> (Point, Point, Point) -> IO ()
fillTri img col (p0, p1, p2) = sequence_ $ zipWith5 (scanline img col) ys x0s x1s z0s z1s
  where
    (bot, mid, top) = (\ls -> (ls!!0, ls!!1, ls!!2)) $ sortBy (compare `on` getY) [p0, p1, p2]
    xb = floor' $ getX bot; xm = floor' $ getX mid; xt = floor' $ getX top
    yb = floor' $ getY bot; ym = floor' $ getY mid; yt = floor' $ getY top
    zb = floor' $ getZ bot; zm = floor' $ getZ mid; zt = floor' $ getZ top
    dx0  = (xt - xb) / (yt - yb)
    dz0  = (zt - zb) / (yt - yb)
    dx1a = (xm - xb) / (ym - yb)
    dz1a = (zm - zb) / (ym - yb)
    dx1b = (xt - xm) / (yt - ym)
    dz1b = (zt - zm) / (yt - ym)
    x0s = enumFromStep xb dx0
    x1s = take (floor (getY mid) - floor (getY bot)) (enumFromStep xb dx1a) ++ enumFromStep xm dx1b
    ys  = [floor yb .. floor yt]
    z0s = enumFromStep zb dz0
    z1s = take (floor (getY mid) - floor (getY bot)) (enumFromStep zb dz1a) ++ enumFromStep zm dz1b
    floor' = fromIntegral . (floor :: Double -> Integer)

enumFromStep :: (Floating a) => a -> a -> [a]
enumFromStep m step = m : go 1
  where go k = let n' = m + k * step
                in n' : go (k + 1)
