{-
Напишите функции, которые вычисляют сумму элементов дерева 
и максимальную высоту дерева:
-}
data Tree a = Leaf | Node (Tree a) a (Tree a) 
  deriving Show

treeSum :: Tree Integer -> Integer
treeSum Leaf = 0
treeSum (Node b a c) = treeSum b + a + treeSum c 
treeHeight :: Tree a -> Int
treeHeight Leaf = 0
treeHeight (Node b a c) = max (treeHeight b) (treeHeight c) + 1 