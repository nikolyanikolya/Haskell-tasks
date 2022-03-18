--Покажите, что всякий аппликативный функтор моноидален. Для этого реализуйте функции unit' и pair'
unit' :: Applicative f => f ()
unit' = pure ()

pair' :: Applicative f => f a -> f b -> f (a,b)
pair' x y = (,) <$> x <*> y