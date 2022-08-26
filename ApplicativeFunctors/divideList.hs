--Функция divideList сворачивает список посредством деления. Модифицируйте ее,
--реализовав divideList' :: (Show a, Fractional a) => [a] -> (String,a), 
--такую что последовательность вычислений отражается в логе:
divideList' :: (Show a, Fractional a) => [a] -> (String,a)
divideList' []     = ("1.0", 1)
divideList' (x:xs) = ("<-"++ show x++"/" ++ fst (divideList' xs), (/) x (snd (divideList' xs)))