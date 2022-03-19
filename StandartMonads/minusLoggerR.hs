--Используя монаду Writer, напишите функцию правой свертки вычитанием
import Control.Monad.Writer 
minusLoggedR :: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedR y [] = do
    tell $ show y
    return y
minusLoggedR y (x:xs) = do
    tell $ "(" ++ show x ++ "-"
    minusLoggedR y xs
    tell ")"
    -- tell $ "(" ++ show x ++ "-"
    -- tell ("-" ++ show y ++ join (replicate (length xs) ")"))
    return (foldr (-) y (x:xs)) 