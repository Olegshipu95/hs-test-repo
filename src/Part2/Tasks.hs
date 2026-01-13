module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)


applyOp :: BinaryOp -> Int -> Int -> Int
applyOp Plus  a b = a + b
applyOp Minus a b = a - b
applyOp Times a b = a * b


-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
a |+| b = BinaryTerm Plus a b
infixl 5 |+|

(|-|) :: Term -> Term -> Term
a |-| b = BinaryTerm Minus a b
infixl 5 |-|


(|*|) :: Term -> Term -> Term
a |*| b = BinaryTerm Times a b
infixl 7 |*|


-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar name replacement term =
  case term of
    IntConstant _ ->
      term

    Variable v ->
      if v == name
        then replacement
        else term

    BinaryTerm op left right ->
      BinaryTerm op
        (replaceVar name replacement left)
        (replaceVar name replacement right)

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate term =
  case term of
    IntConstant _ -> term
    Variable _    -> term
    BinaryTerm op left right ->
      let l = evaluate left
          r = evaluate right
      in case (l, r) of
           (IntConstant lv, IntConstant rv) -> IntConstant (applyOp op lv rv)
           _ -> BinaryTerm op l r
