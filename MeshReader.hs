module MeshReader where

import Data.Maybe (catMaybes)

data ParseElement = VElement Vertex | NElement Normal | FElement Face
  deriving (Eq, Show)

data Vertex = Vertex {vx :: Double, vy :: Double, vz :: Double, vw :: Double}
  deriving (Eq, Show)

data Normal = Normal {nx :: Double, ny :: Double, nz :: Double}
  deriving (Eq, Show)

data Face = Face {a :: Int, b :: Int, c :: Int}
  deriving (Eq, Show)


main :: IO ()
main = print . process . format =<< readFile "objs/teapot.obj"

format :: String -> [[String]]
format xs = map (words . takeWhile (/='#')) $ lines xs

process :: [[String]] -> ([Vertex],[Normal],[Face])
process xs = partitionElements $ catMaybes $ map oneLine xs
  where
    oneLine [] = Nothing
    oneLine xs@(x:_)
      | x == "g"  = Nothing
      | x == "v"  = Just $ VElement $ Vertex (read$xs!!1) (read$xs!!2) (read$xs!!3) 1
      | x == "vn" = Just $ NElement $ Normal (read$xs!!1) (read$xs!!2) (read$xs!!3)
      | x == "f"  = Just $ FElement $ Face   (read$xs!!1) (read$xs!!2) (read$xs!!3)
      | otherwise = error $ "invalid line: " ++ show xs

partitionElements:: [ParseElement] -> ([Vertex],[Normal],[Face])
partitionElements = foldr (myEither left right further) ([],[],[])
 where
   left    a ~(l, r, f) = (a:l, r, f)
   right   a ~(l, r, f) = (l, a:r, f)
   further a ~(l, r, f) = (l, r, a:f)

myEither :: (Vertex -> d) -> (Normal -> d) -> (Face -> d) -> ParseElement -> d
myEither f _ _ (VElement x) =  f x
myEither _ g _ (NElement y) =  g y
myEither _ _ h (FElement z) =  h z
