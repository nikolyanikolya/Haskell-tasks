--Повторите каждый элемент списка заданное число раз.
repeatEveryElem :: Int -> [a] -> [a]
repeatEveryElem _ [] = []
repeatEveryElem a (x:xs) = replicate a x ++ repeatEveryElem a xs