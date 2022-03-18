--Сделайте этот тип функтором и аппликативным функтором с 
-- естественной для векторов семантикой, подобной ZipList.
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
data Triple a = Tr a a a deriving (Eq,Show)
instance Functor Triple where  
  fmap g (Tr x y z) = Tr (g x) (g y) (g z)
instance Applicative Triple where
    pure a = Tr a a a 
    Tr x y z <*> Tr a b c = Tr (x a) (y b) (z c)