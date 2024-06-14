module Knob where

data Knob = Knob KnobType Integer Integer Double Double
  deriving (Eq, Show)

data KnobType
  = Linear
  | EaseInExp
  | EaseOutExp
  | EaseInOutCubic
  | EaseOutElastic Double Double
  | EaseOutBounce Double Double
--  | Logarithmic
--  | Arbitrary
  deriving (Eq, Show)

parseKnob :: Knob -> (Integer -> Either Double Double)
parseKnob (Knob _ x0 x1 y0 y1) n
  | n < x0 = Left y0
  | n > x1 = Left y1
parseKnob (Knob easing x0 x1 y0 y1) n = case easing of
  Linear         -> Right $ y0 + x * (y1 - y0)
  EaseInExp      -> Right $ y0 * (y1 / y0) ** x
  EaseOutExp     -> Right $ y0 + y1 - y1 * (y1 / y0) ** (- x)
  EaseInOutCubic -> Right $ y0 + (y1 - y0) * x * x * (3 - 2 * x)
  EaseOutElastic a b -> Right $ y1 + (y1 - y0) * exp (-a * x) * sin (pi * (b * x - 1) / 2)
  EaseOutBounce a b -> Right $ y1 - (y1 - y0) * exp (-a * x) * abs(sin (pi * (b * x - 1) / 2))
  where x = (fromInteger $ n - x0) / (fromInteger $ x1 - x0)
