--Напишите две реализации функции обращения списка reverse :: [a] -> [a] через свёртки foldr и foldl:
reverse' :: [a] -> [a]
reverse' = foldr fun' ini'
fun' x g = g ++ [x]
ini' = []

reverse'' :: [a] -> [a]

reverse'' = foldl fun'' ini''
fun'' = flip (:)
ini'' = []