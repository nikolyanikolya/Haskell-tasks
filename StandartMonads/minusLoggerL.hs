--Используя монаду Writer, напишите функцию левой свертки вычитанием
import Control.Monad.Writer
minusLoggedL :: (Show a, Num a) => a -> [a] -> Writer String a
reversed :: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedL y xs = reversed y (reverse xs) 
reversed y [] = do
    tell $ show y
    return y
reversed y (x:xs) = do
    tell "("
    reversed y xs
    tell $ "-" ++ show x ++ ")"
    return (foldl (-) y (x:xs))