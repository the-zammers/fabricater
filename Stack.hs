module Stack where

import Data.Maybe (listToMaybe)

peek :: Monoid a => [a] -> a
peek = maybe mempty id . listToMaybe

unsafePeek :: [a] -> Maybe a
unsafePeek = listToMaybe

pop :: [a] -> [a]
pop = drop 1

push :: [a] -> [a]
push (x:xs) = x:x:xs
push [] = []

modHead :: Monoid a => [a] -> a -> [a]
modHead xs y = peek xs <> y : pop xs
