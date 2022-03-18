--Сделайте тип Cmps представителем класса типов Foldable.
newtype Cmps f g x = Cmps { getCmps :: f (g x) } deriving (Eq,Show)
instance (Functor f, Functor g) => Functor (Cmps f g) where
  fmap h (Cmps x) = Cmps (fmap (fmap h) x)
instance (Applicative f, Applicative g) => Applicative (Cmps f g) where
    pure x = Cmps (pure (pure x))
    x <*> a = Cmps ((<*>) <$> getCmps x <*> getCmps a)
instance (Foldable f, Foldable g) => Foldable(Cmps f g) where
      foldMap f (Cmps x) = foldMap (foldMap f) x    

