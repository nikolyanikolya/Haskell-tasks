--Напишите реализации функций из стандартной библиотеки tails, inits :: [a] -> [[a]] через свёртку foldr
tails' :: [a] -> [[a]]
tails' = foldr fun ini
fun x g = (x : head g) : g   
ini = [[]]


inits' :: [a] -> [[a]]
inits' = foldr fun' ini'
fun' x g = [] : map (x:) g
ini' = [[]]