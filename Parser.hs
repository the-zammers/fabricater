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
import Lighting (Material, mkMaterial, Ambient, mkAmbient, PointLight, mkPointLight)

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
  | Box String Point Double Double Double
  | Sphere String Point Double
  | Torus String Point Double Double
  | Push
  | Pop
  | Scale Double Double Double
  | Move Double Double Double
  | Rotate Axis Double
  | Display
  | Save String
  | Clear
  deriving (Eq, Show)

data Symbol
  = MaterialVar String Material
  | KnobVar String
  | AmbientVar Ambient
  | LightVar PointLight
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
  "line"    -> LineT
  "circle"  -> CircleT
  "hermite" -> HermiteT
  "bezier"  -> CBezierT
  "qbezier" -> QBezierT
  "box"     -> BoxT
  "sphere"  -> SphereT
  "torus"   -> TorusT
  "push"    -> PushT
  "pop"     -> PopT
  "scale"   -> ScaleT
  "move"    -> MoveT
  "rotate"  -> RotateT
  "display" -> DisplayT
  "save"    -> SaveT
  "clear"   -> ClearT
  "constants" -> ConstT
  "ambient" -> AmbientT
  "light"   -> LightT
  t         -> maybe (StrLit t) NumLit $ readMaybe t

-- | Convert from a list of tokens to a list of exprs
yacc :: [Token] -> ([Symbol], [Expr])
yacc toks = partitionEithers $ unfoldr expr toks

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
    -> Just (Right $ Box "" (a,b,c) d e f, rest)
  BoxT     : StrLit z
           : NumLit a : NumLit b : NumLit c
           : NumLit d : NumLit e : NumLit f : rest
    -> Just (Right $ Box z (a,b,c) d e f, rest)
  SphereT  : NumLit a : NumLit b : NumLit c
           : NumLit d : rest
    -> Just (Right $ Sphere "" (a,b,c) d, rest)
  SphereT  : StrLit z
           : NumLit a : NumLit b : NumLit c
           : NumLit d : rest
    -> Just (Right $ Sphere z (a,b,c) d, rest)
  TorusT   : NumLit a : NumLit b : NumLit c
           : NumLit d : NumLit e : rest
    -> Just (Right $ Torus "" (a,b,c) d e, rest)
  TorusT   : StrLit z
           : NumLit a : NumLit b : NumLit c
           : NumLit d : NumLit e : rest
    -> Just (Right $ Torus z (a,b,c) d e, rest)
  PushT    : rest
    -> Just (Right $ Push, rest)
  PopT     : rest
    -> Just (Right $ Pop, rest)
  ScaleT   : NumLit a : NumLit b : NumLit c : rest
    -> Just (Right $ Scale a b c, rest)
  MoveT    : NumLit a : NumLit b : NumLit c : rest
    -> Just (Right $ Move a b c, rest)
  RotateT  : StrLit a
           : NumLit b : rest
           | isJust $ (readMaybe (map toUpper a) :: Maybe Axis)
    -> Just (Right $ Rotate (read (map toUpper a)) b, rest)
  DisplayT : rest
    -> Just (Right $ Display, rest)
  SaveT    : StrLit a : rest
    -> Just (Right $ Save a, rest)
  ClearT   : rest
    -> Just (Right $ Clear, rest)
  ConstT   : StrLit z
           : NumLit a : NumLit b : NumLit c
           : NumLit d : NumLit e : NumLit f
           : NumLit g : NumLit h : NumLit i : rest
    -> Just (Left $ MaterialVar z (mkMaterial a b c d e f g h i), rest)
  AmbientT : NumLit a : NumLit b : NumLit c : rest
    -> Just (Left $ AmbientVar (mkAmbient a b c), rest)
  LightT   : NumLit a : NumLit b : NumLit c
           : NumLit d : NumLit e : NumLit f : rest
    -> Just (Left $ LightVar (mkPointLight a b c d e f), rest)
  []
    -> Nothing
  x -> error $ "Parsing error at " ++ show (take 5 x)
