module Part3.Tasks where

import Util (notImplementedYet)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = f n : finc f (n + 1)

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : ff f (f x)

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq nums =
    fst $ maxBy $ map (\item -> (item, length (filter (== item) digits))) digits
    where
        splitIntoDigits :: Int -> [Int]
        splitIntoDigits 0 = []
        splitIntoDigits num = num `mod` 10 : splitIntoDigits (num `div` 10)

        digits = concat (map splitIntoDigits nums)

        maxBy' a b = if snd a > snd b then a else b
        maxBy (head : tail) = foldl maxBy' head tail

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq []     = []
uniq (x:xs) = x : uniq (filter (/= x) xs)

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy _ [] = []
grokBy f (x:xs) =
  (key, x : same) : grokBy f rest
  where
    key  = f x
    same = [ y | y <- xs, f y == key ]
    rest = [ y | y <- xs, f y /= key ]
