-- Реализуйте функцию, находящую элементы следующей 
-- рекуррентной последовательности :
-- b_0 = 1, b_1 = 2, b_2 = 3, b_{k+3} = b_{k+2} - 2b_{k+1} + 3b_{k} 
seqB :: Integer -> Integer
seqB n = helper 1 2 3 n
    where helper acc1 acc2 acc3 n  | n == 0 = acc1
                                   | n == 1 = acc2
                                   | n == 2 = acc3
                                   | n > 2 = helper acc2 acc3 (acc3 -2*acc2 + 3*acc1) (n-1)
                                   | otherwise = error "negative argument"
