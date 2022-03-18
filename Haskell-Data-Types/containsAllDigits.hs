{-
Определите, содержит ли заданное целое число все цифры от 1 до 9. 
(Используйте функцию digits из предыдущего задания.)
-}
import Data.Maybe
import Data.List
digits :: Integer -> [Integer]
digits a  | abs a >= 10 = digits (abs a `div` 10) ++ [abs a `mod` 10]
          | otherwise = [abs a]

containsAllDigits :: Integer -> Bool
containsAllDigits a | Data.Maybe.isJust(find(== 1) list) && 
                        Data.Maybe.isJust(find(== 2) list) && Data.Maybe.isJust(find(== 3) list)  && 
                        Data.Maybe.isJust(find(== 4) list) && Data.Maybe.isJust(find(== 5) list) && 
                        Data.Maybe.isJust(find(== 6) list) && Data.Maybe.isJust(find(== 7) list) && 
                        Data.Maybe.isJust(find(== 8) list) && Data.Maybe.isJust(find(== 9) list) = True
                    |  otherwise = False 
              where list = digits a   