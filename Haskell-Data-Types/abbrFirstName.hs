{-
Реализуйте функцию abbrFirstName, которая 
сокращает имя до первой буквы с 
точкой, то есть если имя было "John", 
то после применения этой функции, оно превратится в "J.". 
Однако если имя было короче двух символов, то оно не меняется.
-}
data Person = Person { firstName :: String, lastName :: String, age :: Int }
    deriving Show
abbrFirstName :: Person -> Person

abbrFirstName p | length (firstName p) >= 2 = Person ((take 1 $ firstName p)  ++ ".") (lastName p) (age p)
                | otherwise =  Person (firstName p) (lastName p) (age p)