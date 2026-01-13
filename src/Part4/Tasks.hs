module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist = foldl (:<) REmpty


-- Реализуйте все представленные ниже классы (см. тесты)
--
--
--
instance Show a => Show (ReverseList a) where
    show = show . rlistToList

instance Eq a => Eq (ReverseList a) where
    REmpty == REmpty = True
    (init1 :< x1) == (init2 :< x2) = x1 == x2 && init1 == init2
    _ == _ = False

instance Semigroup (ReverseList a) where
    xs <> ys = listToRlist $ rlistToList xs ++ rlistToList ys

instance Monoid (ReverseList a) where
    mempty = REmpty
    mappend = (<>)

instance Functor ReverseList where
    fmap _ REmpty = REmpty
    fmap f (init :< x) = fmap f init :< f x

instance Applicative ReverseList where
    pure x = REmpty :< x
    REmpty <*> _ = REmpty
    _ <*> REmpty = REmpty
    fs <*> xs = listToRlist [f x | f <- rlistToList fs, x <- rlistToList xs]

instance Monad ReverseList where
    return = pure
    REmpty >>= _ = REmpty
    (init :< x) >>= f = (init >>= f) <> f x
