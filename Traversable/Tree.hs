--Сделайте двоичное дерево представителем класса типов Traversable
import Data.Traversable (foldMapDefault)
data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)    
instance Functor Tree where
  fmap _ Nil = Nil
  fmap g (Branch x y z) = Branch (fmap g x) (g y) (fmap g z)
instance Applicative Tree where
    pure a = Branch (pure a) a (pure a)
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    Branch x y z <*> Branch a b c = Branch (x <*> a) (y b) (z <*> c)
instance Foldable Tree where
    foldMap = foldMapDefault
instance Traversable Tree where
    traverse g (Branch a b c) = Branch <$> traverse g a <*> g b <*> traverse g c  
    traverse g Nil = pure Nil  