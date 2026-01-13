module Part1.Tasks where

import Util(notImplementedYet)

fact 0 = 1
fact n = n * fact (n - 1)

normalize :: Double -> Double
normalize x = let p = 2*pi in x - p * fromIntegral (floor (x / p))

-- синус числа (формула Тейлора)
-- Формула Тейлора это sin x = x - x^3/3! + x^5/5! и т.д.
mySin :: Double -> Double
mySin x = sum [ term n | n <- [0..10] ]
  where
    y = normalize x
    term n =
      let sign = if even n then 1 else -1
      in fromIntegral sign * y^(2*n + 1) / fromIntegral (fact (2*n + 1))

-- косинус числа (формула Тейлора)
-- Аналогия с sin
myCos :: Double -> Double
myCos x = sum [ term n | n <- [0..20] ]  -- увеличить количество членов
  where
    y = normalize x
    term n =
      let sign = if even n then 1 else -1
      in fromIntegral sign * y^(2*n) / fromIntegral (fact (2*n))


-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a 0 = abs a
myGCD a b = myGCD b (a `mod` b)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year
    | year <= 0 || month < 1 || month > 12 || day < 1 = False
    | month == 2 = day <= if isLeapYear year then 29 else 28
    | month `elem` [4,6,9,11] = day <= 30
    | otherwise = day <= 31

daysInMonth :: Integer -> Integer -> Integer
daysInMonth y m
    | m == 2 = if isLeapYear y then 29 else 28
    | m `elem` [4, 6, 9, 11] = 30
    | otherwise = 31

isLeapYear :: Integer -> Bool
isLeapYear year
    | year `mod` 400 == 0 = True
    | year `mod` 100 == 0 = False
    | year `mod` 4 == 0 = True
    | otherwise = False

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow _ 0 = 1
myPow x n
  | n < 0     = error "error"
  | otherwise = x * myPow x (n - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime n
  | n <= 1    = False
  | otherwise = null [ d | d <- [2..n-1], n `mod` d == 0 ]

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
--shapeArea points = notImplementedYet
shapeArea points = abs (sum terms) / 2
  where
    closed = points ++ [head points]
    terms =
      [ x1*y2 - x2*y1
      | ((x1,y1),(x2,y2)) <- zip closed (tail closed)
      ]

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c
    | not (isTriangle a b c) = -1
    | isRight a b c = 2
    | isAcute a b c = 1
    | otherwise = 0


isTriangle :: Double -> Double -> Double -> Bool
isTriangle a b c =
    a > 0 && b > 0 && c > 0 &&
    a + b > c && a + c > b && b + c > a


isRight :: Double -> Double -> Double -> Bool
isRight a b c =
    isRightAngle a b c || isRightAngle a c b || isRightAngle b c a
  where
    isRightAngle x y z = abs (x*x + y*y - z*z) < 0.000001


isAcute :: Double -> Double -> Double -> Bool
isAcute a b c =
    a*a + b*b > c*c &&
    a*a + c*c > b*b &&
    b*b + c*c > a*a
