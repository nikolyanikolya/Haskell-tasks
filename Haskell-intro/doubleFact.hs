--Определите функцию вычисляющую двойной факториал,
-- то есть произведение натуральных чисел, не превосходящих заданного 
-- числа и имеющих ту же четность. Например: 7!!=7⋅5⋅3⋅1, 8!!=8⋅6⋅4⋅2.
doubleFact :: Integer -> Integer
doubleFact n = helper 1 n
    where helper acc n | n == 0 = acc
                       | n == 1 = acc
                       | n > 1 = helper (acc*n) (n-2)
                       | otherwise = error "negative factorial"