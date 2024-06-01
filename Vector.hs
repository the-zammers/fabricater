module Vector where

data Vec3 a = Vec3 a a a deriving (Eq, Show)

toVec3 :: (a,a,a) -> Vec3 a
toVec3 (x,y,z) = Vec3 x y z

fromVec3 :: Vec3 a -> (a,a,a)
fromVec3 (Vec3 x y z) = (x,y,z)

instance Floating a => Num (Vec3 a) where
  (+) = (<*>) . (fmap (+))
  (*) = (<*>) . (fmap (*))
  negate = fmap negate
  abs = pure . mag
  signum v = fmap (/ mag v) v
  fromInteger = pure . fromInteger

instance Functor Vec3 where
  fmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

instance Applicative Vec3 where
  pure x = Vec3 x x x
  (Vec3 xa ya za) <*> (Vec3 xb yb zb) = Vec3 (xa xb) (ya yb) (za zb)

mag :: Floating a => Vec3 a -> a
mag (Vec3 x y z) = sqrt (x*x + y*y + z*z)

cross :: Num a => Vec3 a -> Vec3 a -> Vec3 a
cross (Vec3 xa ya za) (Vec3 xb yb zb)
  = Vec3 (ya*zb - za*yb) (za*xb - xa*zb) (xa*yb - ya*xb)

dot :: Num a => Vec3 a -> Vec3 a -> a
dot (Vec3 xa ya za) (Vec3 xb yb zb)
  = xa*xb + ya*yb + za*zb

getNormal :: RealFloat a => ((a,a,a), (a,a,a), (a,a,a)) -> Vec3 a
getNormal (p0, p1, p2) = cross veca vecb
  where (v0, v1, v2) = (toVec3 p0, toVec3 p1, toVec3 p2)
        veca = v1 - v0
        vecb = v2 - v0

-- | Checks if a triangle is forward-facing (used for backface culling)
isFrontFace :: RealFloat a => ((a,a,a), (a,a,a), (a,a,a)) -> Bool
isFrontFace (p0, p1, p2) = dot norm view > 0
  where norm = getNormal (p0, p1, p2)
        view = Vec3 0 0 1

costheta :: Floating a => Vec3 a -> Vec3 a -> a
costheta a b = dot (signum a) (signum b)

reflection :: Floating a => Vec3 a -> Vec3 a -> Vec3 a
reflection norm ray = pure (2 * costheta norm ray) * signum norm - signum ray
