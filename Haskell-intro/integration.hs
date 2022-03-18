--Реализуйте функцию, находящую значение определённого интеграла от 
--заданной функции на заданном интервале методом трапеций.
integration f a b | b >= a = (b-a)/1000 * helper f a b 0 0 1
                  | otherwise = (-1)*helper f b a 0 0 1 * (a-b)/1000
    where helper f a b res segment counter  | counter ==1002  = res
                                            | a==b = res
                                            | counter == 1001 = helper f a b (res + (f b)/2) ((b-a)/1000*counter) (counter+1)
                                            | segment == 0 = helper f a b (res + (f a)/2) ((b-a)/1000*counter) (counter+1)
                                            | otherwise = helper f a b (res + f (a + segment)) ((b-a)/1000*counter) (counter+1)