-- | Module: Draw
module Draw where

import Geometry (Point, EdgeList, mkEdge, TriList, mkTri)

-- | Draw a line with endpoints p0 and p1
line :: Point -> Point -> EdgeList
line p0 p1 = [mkEdge p0 p1]

-- | Draw a circle with center c and radius r
circle :: Point -> Double -> EdgeList
circle (cx,cy,cz) r = toEdgeList $ parametric 0.025
  (\t -> r * cos (2*pi*t) + cx)
  (\t -> r * sin (2*pi*t) + cy)
  (const cz)

-- | Draw a cubic Hermite spline
-- | Takes endpoints p0 and p1 and tangents r0 and r1
hermite :: Point -> Point -> Point -> Point -> EdgeList
hermite (p0x,p0y,p0z) (p1x,p1y,p1z) (r0x,r0y,r0z) (r1x,r1y,r1z)
  = toEdgeList $ parametric 0.05
    (\t -> (2*p0x-2*p1x+r0x+r1x)*t*t*t + (-3*p0x+3*p1x-2*r0x-r1x)*t*t + r0x*t + p0x)
    (\t -> (2*p0y-2*p1y+r0y+r1y)*t*t*t + (-3*p0y+3*p1y-2*r0y-r1y)*t*t + r0y*t + p0y)
    (\t -> (2*p0z-2*p1z+r0z+r1z)*t*t*t + (-3*p0z+3*p1z-2*r0z-r1z)*t*t + r0z*t + p0z)

-- | Draw a cubic Bezier spline
-- | Takes endpoints p0 and p3 and control points p1 and p2
cbezier :: Point -> Point -> Point -> Point -> EdgeList
cbezier (p0x,p0y,p0z) (p1x,p1y,p1z) (p2x,p2y,p2z) (p3x,p3y,p3z)
  = toEdgeList $ parametric 0.05
    (\t -> (-p0x+3*p1x-3*p2x+p3x)*t*t*t + (3*p0x-6*p1x+3*p2x)*t*t + (-3*p0x+3*p1x)*t + p0x)
    (\t -> (-p0y+3*p1y-3*p2y+p3y)*t*t*t + (3*p0y-6*p1y+3*p2y)*t*t + (-3*p0y+3*p1y)*t + p0y)
    (\t -> (-p0z+3*p1z-3*p2z+p3z)*t*t*t + (3*p0z-6*p1z+3*p2z)*t*t + (-3*p0z+3*p1z)*t + p0z)

-- | Draw a quadratic Bezier spline
-- | Takes endpoints p0 and p2 and control point p1
qbezier :: Point -> Point -> Point -> EdgeList
qbezier (p0x,p0y,p0z) (p1x,p1y,p1z) (p2x,p2y,p2z)
  = toEdgeList $ parametric 0.05
    (\t -> (p0x-2*p1x+p2x)*t*t + (-2*p0x+2*p1x)*t + p0x)
    (\t -> (p0y-2*p1y+p2y)*t*t + (-2*p0y+2*p1y)*t + p0y)
    (\t -> (p0z-2*p1z+p2z)*t*t + (-2*p0z+2*p1z)*t + p0z)

-- | Draw a parametric function
-- | Takes one timestep and three functions of t
parametric :: Double -> (Double -> Double) -> (Double -> Double) -> (Double -> Double) -> [Point]
parametric step fx fy fz = go <$> [0, step .. 1]
  where go t = (fx t, fy t, fz t)

-- | Draw a parametric functions
-- | Takes two timesteps and three funcions of t1 and t2
parametric2 :: Double -> Double -> (Double -> Double -> Double) -> (Double -> Double -> Double) -> (Double -> Double -> Double) -> [[Point]]
parametric2 step1 step2 fx fy fz = outer go [0, step1 .. 1] [0, step2 .. 1]
  where go t1 t2 = (fx t1 t2, fy t1 t2, fz t1 t2)
        outer f x1 x2 = flip map (f <$> x1) (<$> x2)

-- | Draw a rectangular prism
-- | Defined with upper-left-front vertex, width, height, and depth
box :: Point -> Double -> Double -> Double -> TriList
box (x,y,z) w h d
  = map (\(px,py,pz) -> mkTri (pts!!px) (pts!!py) (pts!!pz)) $ [(0,3,1), (3,0,2), (1,7,5), (7,1,3), (5,6,4), (6,5,7), (4,2,0), (2,4,6), (4,1,5), (1,4,0), (7,3,2), (2,6,7)]
  where
    pts = [(x,y,z  ), (x+w,y,z  ), (x,y-h,z  ), (x+w,y-h,z  ),
           (x,y,z-d), (x+w,y,z-d), (x,y-h,z-d), (x+w,y-h,z-d)]

-- | Draw a sphere with center c and radius r
sphere :: Point -> Double -> TriList
sphere (x,y,z) r
  = toTriList True $ parametric2 0.0125 0.0125
    (\_ t -> r * cos (pi*t)                + x)
    (\p t -> r * sin (pi*t) * cos (2*pi*p) + y)
    (\p t -> r * sin (pi*t) * sin (2*pi*p) + z)

-- | Draw a torus with center c, cross-section radius r1, and torus radius r2
torus :: Point -> Double -> Double -> TriList
torus (x,y,z) r1 r2
  = toTriList False $ parametric2 0.0125 0.025
    (\p t ->   cos (2*pi*p) * (r1 * cos (2*pi*t) + r2) + x)
    (\_ t ->                   r1 * sin (2*pi*t)       + y)
    (\p t ->   sin (2*pi*p) * (r1 * cos (2*pi*t) + r2) + z)

-- | Convert from a list of points to an edgelist
-- | Connects adjacent points, so will generate n-1 lines given n points
-- | *This function is partial: Will fail on the empty list*
toEdgeList :: [Point] -> EdgeList
toEdgeList = zipWith mkEdge <*> tail

-- | Convert from a list of lists of points to a triangle list
-- | Assumes adjacent lists are adjacent lines of points to weave
toTriList :: Bool -> [[Point]] -> TriList
toTriList isSphere (m:n:mns) = foldr f [] [0 .. length m-2] ++ toTriList isSphere (n:mns)
  where f p ls 
          | isSphere && p   == 0        = triA p : ls
          | isSphere && p+2 == length m = triB p : ls
          | otherwise = triA p : triB p : ls
        triA p = mkTri (m!!p) (m!!(p+1)) (n!!(p+1)) 
        triB p = mkTri (m!!p) (n!!(p+1)) (n!!p) 
toTriList _ _ = []
