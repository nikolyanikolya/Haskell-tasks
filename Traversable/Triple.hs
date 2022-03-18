--сделайте Triple из предыдущего домашнего задания представителем классов типов Foldable и Traversable
import Data.Traversable (foldMapDefault)
data Triple a = Tr a a a deriving (Eq,Show)
instance Functor Triple where  
  fmap g (Tr x y z) = Tr (g x) (g y) (g z)
instance Applicative Triple where
  pure a = Tr a a a 
  Tr x y z <*> Tr a b c = Tr (x a) (y b) (z c)
instance Foldable Triple where
  foldMap = foldMapDefault
instance Traversable Triple where
  traverse g (Tr a b c) = Tr <$> g a <*> g b <*> g c