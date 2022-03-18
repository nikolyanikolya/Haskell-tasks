--Ассоциативным списком называют список пар (ключ, значение). Реализуйте функцию поиска в таком списке, 
--возвращающую список всех значений с заданным ключом. Используйте монаду списка и do-нотацию:
lookups :: (Eq k) => k -> [(k,v)] -> [v]
lookups x ys = do 
    y <- ys
    True <-  [fst y == x]
    return (snd y)