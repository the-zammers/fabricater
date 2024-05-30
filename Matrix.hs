{-# LANGUAGE DataKinds, KindSignatures, ScopedTypeVariables, FlexibleInstances  #-}

module Matrix where

import GHC.TypeNats (Nat, KnownNat)
import GHC.TypeLits (natVal)
import Data.Proxy (Proxy (Proxy))
import Data.Array ((!))
import qualified Data.Array as A

-- | Safe matrix type
data Matrix (m :: Nat) (n :: Nat) a = Matrix (Matr a)
  deriving (Eq, Ord, Read, Show)
-- | Unsafe underlying matrix type
type Matr a = A.Array (Integer,Integer) a

-- | Square matrix typeclass instances
instance (KnownNat m, Num v) => Semigroup (Matrix m m v) where
  a <> b = matrixMult a b

instance (KnownNat m, Num v) => Monoid (Matrix m m v) where
  mempty = identity

-- | Creates an identity matrix
identity :: forall n v. (KnownNat n, Num v) => Matrix n n v
identity = Matrix $ A.array ((0,0), (dim-1, dim-1)) [((i,j), diag i j) | i <- [0..dim-1], j <- [0..dim-1]]
  where dim = natVal (Proxy :: Proxy n)
        diag x y = if x==y then 1 else 0

-- | Creates an arbitrary matrix given a list of elements
listToMatrix :: forall m n v. (KnownNat m, KnownNat n) => [v] -> Matrix m n v
listToMatrix ls = Matrix $ A.listArray ((0,0), (m'-1,n'-1)) ls
  where m' = natVal (Proxy :: Proxy m)
        n' = natVal (Proxy :: Proxy n)

matrixToList :: Matrix m n v -> [v]
matrixToList (Matrix arr) = A.elems arr

-- | Multiplies two matrices
matrixMult :: forall m n o v. (KnownNat m, KnownNat o, Num v) => Matrix m n v -> Matrix n o v -> Matrix m o v
matrixMult (Matrix a) (Matrix b) = Matrix $ A.array ((0,0), (m'-1,o'-1)) [((i,j), sum $ zipWith (*) (getRow i a) (getCol j b)) | i <- [0..m'-1], j <- [0..o'-1]]
  where m' = natVal (Proxy :: Proxy m)
        o' = natVal (Proxy :: Proxy o)

-- | Prints a matrix
printMatrix :: Show v => Matrix m n v -> IO ()
printMatrix (Matrix arr) = mapM_ (putStrLn . unwords . map show) $ [[ arr ! (i,j) | j <- [startc..endc]] | i <- [startr..endr]]
  where ((startr,startc), (endr,endc)) = A.bounds arr

-- | Gets all elements in a given row of a matrix
getRow :: Integer -> Matr v -> [v]
getRow n arr = map snd $ filter (\((a,_), _) -> a==n) $ A.assocs arr

-- | Gets all elements in a given column of a matrix
getCol :: Integer -> Matr v -> [v]
getCol n arr = map snd $ filter (\((_,b), _) -> b==n) $ A.assocs arr
