module Knob where

data Knob = Knob KnobType Integer Integer Double Double
  deriving (Eq, Show)

data KnobType
  = Constant
  | Linear
  | EaseInExp
  | EaseOutExp
  | EaseInOutCubic
  | EaseOutElastic Double Double
  | EaseOutBounce Double Double
  deriving (Eq, Show)

parseKnob :: Knob -> (Integer -> Either (Integer, Double) Double)
parseKnob (Knob easing x0 x1 y0 y1) n
  | easing == Constant = Right y0
  | n < x0 = Left (-x0, y0)
  | n > x1 = Left (x1, y1)
  | otherwise = case easing of
    Constant       -> Right $ y0 -- already covered
    Linear         -> Right $ y0 + x * (y1 - y0)
    EaseInExp      -> catch y0 y1 $ Right $ y0 * (y1 / y0) ** x
    EaseOutExp     -> catch y0 y1 $ Right $ y0 + y1 - y1 * (y1 / y0) ** (- x)
    EaseInOutCubic -> Right $ y0 + (y1 - y0) * x * x * (3 - 2 * x)
    EaseOutElastic a b -> Right $ y1 + (y1 - y0) * exp (-a * x) * sin (pi * (b * x - 1) / 2)
    EaseOutBounce a b -> Right $ y1 - (y1 - y0) * exp (-a * x) * abs (sin (pi * (b * x - 1) / 2))
  where x = (fromInteger $ n - x0) / (fromInteger $ x1 - x0)
        catch a b = if signum a == signum b then id else error "Error: knob start and end values have incompatible signs for the given rate of change"
