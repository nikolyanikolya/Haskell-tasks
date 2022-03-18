--Покажите, что всякий моноидальный функтор аппликативен
pure' :: Monoidal f => a -> f a
pure' x = fmap ((\x y -> x) x) unit 

ap' :: Monoidal f => f (a -> b) -> f a -> f b
ap' f x =  uncurry ($) <$> (f *&* x)