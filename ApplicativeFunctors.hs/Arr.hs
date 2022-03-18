--Сделайте типы данных Arr2 e1 e2 и Arr3 e1 e2 e3 представителями классов типов Functor и Applicative
newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }

newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }

instance Functor (Arr2 e1 e2) where
  fmap a (Arr2 b) = Arr2 (\e1 e2 -> a (b e1 e2))

instance Functor (Arr3 e1 e2 e3) where
  fmap a (Arr3 b) = Arr3 (\e1 e2 e3 -> a (b e1 e2 e3))

instance Applicative (Arr2 e1 e2) where
  pure x = Arr2 (pure(pure x))
  (Arr2 a) <*> (Arr2 b) = Arr2 (\e1 e2 -> a e1 e2 (b e1 e2))

instance Applicative (Arr3 e1 e2 e3) where
  pure x = Arr3(pure(pure(pure x )))
  (Arr3 a) <*> (Arr3 b) = Arr3 (\e1 e2 e3 -> a e1 e2 e3 (b e1 e2 e3))