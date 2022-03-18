-- Реализуйте функцию, находящую сумму и количество цифр 
-- заданного целого числа.
sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x | abs x `div` 10 == 0 = (abs x, 1)
           | otherwise = (fst (sum'n'count (abs x `div` 10)) + 
           (abs x `mod` 10), snd (sum'n'count (abs x `div` 10))+1)  