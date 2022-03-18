--Сформируйте список цифр заданного целого числа.
digits :: Integer -> [Integer]

digits a  | abs a >= 10 = digits (abs a `div` 10) ++ [abs a `mod` 10]
          | otherwise = [abs a]