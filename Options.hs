-- | Module: Options
module Options where

import Control.Applicative (Alternative(..))
import Data.List (unfoldr)

-- | Command-line options datatype to return to main
data Options x = Options {silent :: Bool, willRun :: Bool, getScript :: Maybe x, getOutput :: Maybe x, getDisp :: Maybe x}

instance Functor Options where
  fmap f (Options x y s o d) = Options x y (fmap f s) (fmap f o) (fmap f d)
  
instance Applicative Options where
  pure a = Options True True (pure a) (pure a) (pure a)
  (Options x0 y0 s0 o0 d0) <*> (Options x1 y1 s1 o1 d1) = Options (x0&&x1) (y0&&y1) (s0<*>s1) (o0<*>o1) (d0<*>d1)

instance Alternative Options where
  empty = Options True True empty empty empty
  (Options x0 y0 s0 o0 d0) <|> (Options x1 y1 s1 o1 d1) = Options (x0&&x1) (y0&&y1) (s0<|>s1) (o0<|>o1) (d0<|>d1)

-- | Takes a list of command-line options and return an Options
parseArgs :: [String] -> Options String
parseArgs = foldr (<|>) empty . unfoldr go
  where go [] = Nothing
        go ("--script" :x:xs) = Just (empty {getScript=Just x}, xs)
        go ("--output" :x:xs) = Just (empty {silent=False, getOutput=Just x}, xs)
        go ("-O"         :xs) = Just (empty {silent=False}, xs)
        go ("--display":x:xs) = Just (empty {getDisp=Just x}, xs)
        go ("-D"         :xs) = Just (empty {willRun=False}, xs)
        go ("-f"       :x:xs) = go ("--script":x:xs)
        go ("-o"       :x:xs) = go ("--output":x:xs)
        go ("-d"       :x:xs) = go ("--display":x:xs)
        go (x:_) = error $ "invalid command-line argument: " ++ show x
