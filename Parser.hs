{-# LANGUAGE FlexibleInstances #-}

-- | Module: Parser

module Parser where

-- try using Text.ParserCombinatorsReadP

import Prelude hiding (lex)
import Text.Read (readMaybe)
import Data.Maybe (isJust)
import Data.Either (partitionEithers)
import Data.Char (toUpper)
import Data.List (unfoldr)

import Geometry (Axis(..), Point)
import Color (RGB(..))
import Vector (Vec3(..))
import Lighting (Material(..), Ambient(..), PointLight(..))

-- | All valid tokens
data Token
  = LineT
  | CircleT
  | HermiteT
  | CBezierT
  | QBezierT
  | BoxT
  | SphereT
  | TorusT
  | PushT
  | PopT
  | ScaleT
  | MoveT
  | RotateT
  | DisplayT
  | SaveT
  | ClearT
  | ConstT
  | AmbientT
  | LightT
  | FramesT
  | BasenameT
  | VaryT
  | NumLit Double
  | StrLit String
  deriving (Eq, Show)

-- | All valid expressions
data Expr
  = Line Point Point
  | Circle Point Double
  | Hermite Point Point Point Point
  | CBezier Point Point Point Point
  | QBezier Point Point Point
  | Box (Maybe String) Point Double Double Double
  | Sphere (Maybe String) Point Double
  | Torus (Maybe String) Point Double Double
  | Push
  | Pop
  | Scale (Maybe String) Double Double Double
  | Move (Maybe String) Double Double Double
  | Rotate (Maybe String) Axis Double
  | Display
  | Save String
  | Clear
  deriving (Eq, Show)

data Symbol
  = MaterialVar String Material
  | KnobVar String Knob
  | AmbientVar Ambient
  | LightVar PointLight
  | FramesVar Integer
  | BasenameVar String
  deriving (Eq, Show)

data Knob = Knob Integer Integer Double Double
  deriving (Eq, Show)

-- | Convert from an input String to a list of Exprs
parse :: String -> ([Symbol], [Expr])
parse = yacc . lex

-- | Convert from an input String to a list of Tokens
lex :: String -> [Token]
-- WRONG: ONLY LOOKING FOR ONE SLASH
lex = map token . concatMap (words . takeWhile (/='/')) . lines

-- | Convert from a word in a String to a Token
token :: String -> Token
token = \case
  "line"      -> LineT
  "circle"    -> CircleT
  "hermite"   -> HermiteT
  "bezier"    -> CBezierT
  "qbezier"   -> QBezierT
  "box"       -> BoxT
  "sphere"    -> SphereT
  "torus"     -> TorusT
  "push"      -> PushT
  "pop"       -> PopT
  "scale"     -> ScaleT
  "move"      -> MoveT
  "rotate"    -> RotateT
  "display"   -> DisplayT
  "save"      -> SaveT
  "clear"     -> ClearT
  "constants" -> ConstT
  "ambient"   -> AmbientT
  "light"     -> LightT
  "frames"    -> FramesT
  "basename"  -> BasenameT
  "vary"      -> VaryT
  t           -> maybe (StrLit t) NumLit $ readMaybe t

-- | Convert from a list of tokens to a list of exprs and a symbol table
yacc :: [Token] -> ([Symbol], [Expr])
yacc = partitionEithers . unfoldr expr

-- | yacc helper
expr :: [Token] -> Maybe (Either Symbol Expr, [Token])
expr = \case
  LineT    : NumLit a : NumLit b : NumLit c
           : NumLit d : NumLit e : NumLit f : rest
    -> Just (Right $ Line (a,b,c) (d,e,f), rest)
  CircleT  : NumLit a : NumLit b : NumLit c
           : NumLit d : rest
    -> Just (Right $ Circle (a,b,c) d, rest)
  HermiteT : NumLit a : NumLit b
           : NumLit c : NumLit d
           : NumLit e : NumLit f
           : NumLit g : NumLit h : rest
    -> Just (Right $ Hermite (a,b,0) (c,d,0) (e,f,0) (g,h,0), rest)
  CBezierT : NumLit a : NumLit b
           : NumLit c : NumLit d
           : NumLit e : NumLit f
           : NumLit g : NumLit h : rest
    -> Just (Right $ CBezier (a,b,0) (c,d,0) (e,f,0) (g,h,0), rest)
  QBezierT : NumLit a : NumLit b
           : NumLit c : NumLit d
           : NumLit e : NumLit f : rest
    -> Just (Right $ QBezier (a,b,0) (c,d,0) (e,f,0), rest)
  BoxT     : NumLit a : NumLit b : NumLit c
           : NumLit d : NumLit e : NumLit f : rest
    -> Just (Right $ Box Nothing (a,b,c) d e f, rest)
  BoxT     : StrLit z
           : NumLit a : NumLit b : NumLit c
           : NumLit d : NumLit e : NumLit f : rest
    -> Just (Right $ Box (Just z) (a,b,c) d e f, rest)
  SphereT  : NumLit a : NumLit b : NumLit c
           : NumLit d : rest
    -> Just (Right $ Sphere Nothing (a,b,c) d, rest)
  SphereT  : StrLit z
           : NumLit a : NumLit b : NumLit c
           : NumLit d : rest
    -> Just (Right $ Sphere (Just z) (a,b,c) d, rest)
  TorusT   : NumLit a : NumLit b : NumLit c
           : NumLit d : NumLit e : rest
    -> Just (Right $ Torus Nothing (a,b,c) d e, rest)
  TorusT   : StrLit z
           : NumLit a : NumLit b : NumLit c
           : NumLit d : NumLit e : rest
    -> Just (Right $ Torus (Just z) (a,b,c) d e, rest)
  PushT    : rest
    -> Just (Right $ Push, rest)
  PopT     : rest
    -> Just (Right $ Pop, rest)
  ScaleT   : NumLit a : NumLit b : NumLit c
           : StrLit z : rest
    -> Just (Right $ Scale (Just z) a b c, rest)
  ScaleT   : NumLit a : NumLit b : NumLit c : rest
    -> Just (Right $ Scale Nothing a b c, rest)
  MoveT    : NumLit a : NumLit b : NumLit c
           : StrLit z : rest
    -> Just (Right $ Move (Just z) a b c, rest)
  MoveT    : NumLit a : NumLit b : NumLit c : rest
    -> Just (Right $ Move Nothing a b c, rest)
  RotateT  : StrLit a
           : NumLit b
           : StrLit z : rest
           | isJust $ (readMaybe (map toUpper a) :: Maybe Axis)
    -> Just (Right $ Rotate (Just z) (read (map toUpper a)) b, rest)
  RotateT  : StrLit a
           : NumLit b : rest
           | isJust $ (readMaybe (map toUpper a) :: Maybe Axis)
    -> Just (Right $ Rotate Nothing (read (map toUpper a)) b, rest)
  DisplayT : rest
    -> Just (Right $ Display, rest)
  SaveT    : StrLit a : rest
    -> Just (Right $ Save a, rest)
  ClearT   : rest
    -> Just (Right $ Clear, rest)
  ConstT   : StrLit z
           : NumLit kar : NumLit kdr : NumLit ksr
           : NumLit kag : NumLit kdg : NumLit ksg
           : NumLit kab : NumLit kdb : NumLit ksb : rest
    -> Just (Left $ MaterialVar z (Material (RGB kar kag kab) (RGB kdr kdg kdb) (RGB ksr ksg ksb)), rest)
  AmbientT : NumLit a : NumLit b : NumLit c : rest
    -> Just (Left $ AmbientVar (Ambient (RGB a b c)), rest)
  LightT   : NumLit r : NumLit g : NumLit b
           : NumLit x : NumLit y : NumLit z : rest
    -> Just (Left $ LightVar (PointLight (Vec3 x y z) (RGB r g b)), rest)
  FramesT  : NumLit a : rest
           | a == fromInteger (round a)
    -> Just (Left $ FramesVar (round a), rest)
  BasenameT : StrLit a : rest
    -> Just (Left $ BasenameVar a, rest)
  VaryT    : StrLit a
           : NumLit sFrame : NumLit eFrame
           : NumLit sValue : NumLit eValue : rest
           | sFrame == fromInteger (round sFrame) && eFrame == fromInteger (round eFrame)
    -> Just (Left $ KnobVar a (Knob (round sFrame) (round eFrame) sValue eValue), rest)
  []
    -> Nothing
  x -> error $ "Parsing error at " ++ show (take 5 x)
