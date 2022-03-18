--Напишите представителей моноидального функтора для Maybe, пары и ((->) e):
class Functor f => Monoidal f where
  unit  :: f ()
  (*&*) :: f a -> f b -> f (a,b)

instance Monoidal Maybe where
  unit = Just ()
  (*&*) (Just x) (Just y) = Just (x, y) 
  (*&*) _ Nothing = Nothing
  (*&*) Nothing _ = Nothing
instance Monoid s => Monoidal ((,) s) where
  unit = (mempty, ())
  (*&*) (x, y) (a, b) = (x<>a, (y, b))
instance Monoidal ((->) e) where
  unit f = ()
  (*&*) f g = \y -> (f y, g y)