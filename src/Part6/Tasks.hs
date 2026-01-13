{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import qualified Data.Map as Map
import Util (notImplementedYet)

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix
  { sparseMatrixWidth :: Int
  , sparseMatrixHeight :: Int
  , sparseMatrixElements :: Map.Map (Int, Int) a
  } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями, 
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix m where
  fromLists :: [[Int]] -> m
  toLists :: m -> [[Int]]

instance Matrix Int where
  fromLists [[x]] = x
  fromLists _ = error "Int can only represent 1x1 matrix"
  toLists x = [[x]]

instance Matrix [[Int]] where
  fromLists = id
  toLists = id

instance Matrix (SparseMatrix Int) where
  fromLists xs = SparseMatrix w h elems
    where
      h = length xs
      w = if null xs then 0 else length (head xs)
      elems = Map.fromList [ ((i,j), x) | (i,row) <- zip [0..] xs
                                        , (j,x) <- zip [0..] row
                                        , x /= 0 ]
  toLists (SparseMatrix w h m) = 
    [ [ Map.findWithDefault 0 (i,j) m | j <- [0..w-1]] | i <- [0..h-1] ]

-- Определите экземпляры данного класса для: 
-- * числа (считается матрицей 1x1) 
-- * списка списков чисел 
-- * типа SparseMatrix, представленного выше
eye :: Matrix m => Int -> m
eye n = fromLists [ [if i == j then 1 else 0 | j <- [0..n-1]] | i <- [0..n-1]]

zero :: Matrix m => Int -> Int -> m
zero w h = fromLists (replicate h (replicate w 0))

multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix a b = fromLists [ [ sum $ zipWith (*) row col | col <- transpose (toLists b) ]
                                | row <- toLists a ]
  where
    transpose ([]:_) = []
    transpose x = map head x : transpose (map tail x)

determinant :: Matrix m => m -> Int
determinant mx = det (toLists mx)
  where
    det [[x]] = x
    det m = sum [ (-1)^j * head m !! j * det (minor 0 j m) | j <- [0..length m - 1] ]
    minor i j mtx = [ [ mtx !! r !! c | c <- [0..length (mtx !! r) - 1], c /= j ]
                     | r <- [0..length mtx - 1], r /= i ]

