{-# LANGUAGE FlexibleInstances #-}

-- | Module: Color
module Color where

-- | Color type to use everywhere else
type Color = RGB (Channel Integer)

-- | Ensure value stays in bounds
clamp :: (Ord a, Bounded a) => a -> a
clamp chan
  | chan < minBound = minBound
  | chan > maxBound = maxBound
  | otherwise = chan

-- | The maximum value the Channel can reach (255)
maxValue :: Integer
maxValue = 255

-- | Arbitrary component of a single color
newtype Channel a = Channel {unpack :: a}
  deriving (Eq, Ord, Read)

instance Bounded (Channel Double) where
  minBound = Channel 0
  maxBound = Channel 255

instance (Num a) => Bounded (Channel a) where
  minBound = Channel 0
  maxBound = Channel 255

instance (Ord a, Num a) => Semigroup (Channel a) where
  (<>) = (+)

instance (Ord a, Num a) => Monoid (Channel a) where
  mempty = minBound

instance Show a => Show (Channel a) where
  show = show . unpack

instance (Ord a, Num a) => Num (Channel a) where
  -- do NOT coerce (+) or (*) because you'll lose proper clamping
  (Channel chan1) + (Channel chan2) = clamp . Channel $ chan1 + chan2
  (Channel chan1) * (Channel chan2) = clamp . Channel $ chan1 * chan2
  abs = id
  signum x = if x==0 then 0 else 1
  negate chan = maxBound - chan
  fromInteger = clamp . Channel . fromInteger
  -- negate does NOT give the additive inverse, subtract hangs forever


-- | A color triple with red, green, and blue
data RGB a = RGB {red :: !a, green :: !a, blue :: !a}
  deriving (Eq, Ord, Bounded, Read)

instance (Semigroup a) => Semigroup (RGB a) where
  (<>) = (<*>) . fmap (<>)

instance (Monoid a) => Monoid (RGB a) where
  mempty = pure mempty

instance Functor RGB where
  fmap f (RGB r g b) = RGB (f r) (f g) (f b)

instance Applicative RGB where
  pure c = RGB c c c
  (RGB fr fg fb) <*> (RGB r g b) = RGB (fr r) (fg g) (fb b)

instance (Show a) => Show (RGB a) where
  show (RGB r g b) = show r ++ " " ++ show g ++ " " ++ show b

instance (Ord a, Num a) => Num (RGB a) where
  (+) rgb1 rgb2 = (+) <$> rgb1 <*> rgb2
  (*) rgb1 rgb2 = (*) <$> rgb1 <*> rgb2
  abs = fmap abs
  signum = fmap signum
  negate = fmap negate
  fromInteger = fmap fromInteger . pure

-- | Create a new Color
readColor :: Int -> Int -> Int -> Color
readColor r g b = fromIntegral <$> RGB r g b

-- | List of common, easily distinguishable colors
colList :: [Color]
colList = [readColor r g b | r <- [0,85..255], g <- [0,85..255], b <- [0,85..255]]
