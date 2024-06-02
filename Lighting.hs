module Lighting where

import Color
import Vector

type Lights = (Ambient, [PointLight])

data Ambient = Ambient { colA :: RGB Double }
  deriving (Eq, Show)

data PointLight = PointLight { loc :: Vec3 Double
                             , col :: RGB Double
                             }
  deriving (Eq, Show)


data Material = Material { ka :: RGB Double
                         , kd :: RGB Double
                         , ks :: RGB Double
                         }
  deriving (Eq, Show)

getLighting :: Lights -> Material -> ((Double, Double, Double), (Double, Double, Double), (Double, Double, Double)) -> Color
getLighting (a, ps) mat tri = fmap (fromInteger . round)
  $ sum (map (
    \p -> col p * ks mat * pure (max 0 $ dot (reflection norm (loc p)) (signum view)) ^ (4 :: Integer)
        + col p * kd mat * pure (max 0 $ costheta norm (loc p))
    ) ps)
  + colA a * ka mat
  where norm = getNormal tri
        view = toVec3 (0, 0, 1)
