{-
Дан список (возможно бесконечный) и положительное целое число n. Создайте спиcок "скользящих" 
 подсписков длины nn, то есть список списков следующего вида: 
 [[a_1 ... a_n], [a_2 ... a_{n+1}], [a_3 ... a_{n+2}] ... ]
-}
sublist :: Int -> Int -> [a] -> [a]
sublist fr to s = take  (to - max fr 0) $ drop fr s
movingLists :: Int -> [a] -> [[a]]
movingLists _ [] = [] 
movingLists k (x:xs) | length(take k (x:xs))>=k = sublist 0 k (x:xs) : movingLists k xs
                     | otherwise = []