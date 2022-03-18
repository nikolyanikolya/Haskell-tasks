--Сделайте двоичное дерево функтором и аппликативным функтором, 
--реализовав в последнем случае семантику применения узла к соответствующему узлу второго дерева
data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)    
instance Functor Tree where
  fmap _ Nil = Nil
  fmap g (Branch x y z) = Branch (fmap g x) (g y) (fmap g z)
instance Applicative Tree where
    pure a = Branch (pure a) a (pure a)
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    Branch x y z <*> Branch a b c = Branch (x <*> a) (y b) (z <*> c)