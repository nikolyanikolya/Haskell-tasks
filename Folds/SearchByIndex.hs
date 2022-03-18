--Напишите реализацию оператора "безопасного" поиска элемента списка по индексу 
--(!!!) :: [a] -> Int -> Maybe a через foldr:
(!!!) :: [a] -> Int -> Maybe a
xs !!! n = foldr fun ini xs n
fun x g n | n>0 = g (n-1)
          | n<0 = Nothing
          | otherwise = Just x
ini = const Nothing
