module Lighting where

import Color
import Vector

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

getLighting :: (Ambient, [PointLight], Material) -> ((Double, Double, Double), (Double, Double, Double), (Double, Double, Double)) -> Color
getLighting (a, (p:ps), mat) tri = fmap (fromInteger . round)
                $ col p * ks mat * pure (max 0 $ dot (reflection norm (loc p)) (signum view)) ^ (2 :: Integer)
                + col p * kd mat * pure (max 0 $ costheta norm (loc p))
                + colA a * ka mat
  where norm = getNormal tri
        view = toVec3 (0, 0, 1)

mkAmbient :: Double -> Double -> Double -> Ambient
mkAmbient r g b = Ambient $ RGB r g b

mkPointLight :: Double -> Double -> Double -> Double -> Double -> Double -> PointLight
mkPointLight r g b x y z = PointLight (Vec3 x y z) (RGB r g b)

mkMaterial :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Material
mkMaterial kar kdr ksr kag kdg ksg kab kdb ksb = Material (RGB kar kag kab) (RGB kdr kdg kdb) (RGB ksr ksg ksb)
