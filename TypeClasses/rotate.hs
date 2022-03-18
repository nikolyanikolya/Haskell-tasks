-- Реализуйте функцию, задающую циклическое вращение списка.
rotate :: Int -> [a] -> [a]
rotate 0 x = x
rotate _ [] = []
rotate n xs | length (take n xs)==n && n>0 = drop n xs ++ take n xs
            | length (take n xs) /= n && n>0 = drop (n `mod` length xs) xs ++ take (n `mod` length xs) xs
            | otherwise = drop (length xs - (abs n `mod`  length xs)) xs ++ take (length xs - (abs n `mod`  length xs)) xs