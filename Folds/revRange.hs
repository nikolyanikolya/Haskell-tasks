{-
Используя unfoldr, реализуйте функцию, которая возвращает в обратном 
алфавитном порядке список символов, попадающих в заданный парой диапазон. 
Попадание символа x в диапазон пары (a,b) означает, что x >= a и x <= b.
-}
import Data.List (unfoldr)
revRange :: (Char,Char) -> [Char]
revRange = unfoldr fun
fun p = if uncurry (<=) p then Just (snd p, ((fst p), pred (snd p))) else Nothing