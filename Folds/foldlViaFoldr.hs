--Напишите реализацию foldl через foldr
foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' f v xs = foldr (fun f) ini xs v
ini = id
fun f a g y = g (f y a)