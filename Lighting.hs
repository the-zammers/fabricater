module Lighting where

import Color
import Vector

data PointLight = PointLight { loc :: Vec3 Double
                             , col :: RGB Double
                             }


data Material = Material { ka :: RGB Double
                         , kd :: RGB Double
                         , ks :: RGB Double
                         }
  deriving (Eq, Show)

getLighting :: Material -> ((Double, Double, Double), (Double, Double, Double), (Double, Double, Double)) -> Color
getLighting mat tri = fmap (fromInteger . round)
                $ col p * ks mat * pure (max 0 $ dot (reflection norm (loc p)) (signum view)) ^ (2 :: Integer)
                + col p * kd mat * pure (max 0 $ costheta norm (loc p))
                + a * ka mat
  where norm = getNormal tri
        view = toVec3 (0, 0, 1)
        a = RGB 100 120 150
        p = PointLight (toVec3 (-0.5, 1, 1)) (RGB 241 241 241)

mkMaterial :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Material
mkMaterial kar kdr ksr kag kdg ksg kab kdb ksb = Material (RGB kar kag kab) (RGB kdr kdg kdb) (RGB ksr ksg ksb)
