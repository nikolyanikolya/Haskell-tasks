{- сделайте Matrix представителем класса типов Show. Строки матрицы (внутренние списки) должны изображаться как списки; 
каждый следующий внутренний список должен начинаться с новой строки 
(используйте символ'\n' в качестве разделителя). 
Пустая матрица должна выводиться как EMPTY.
-}
newtype Matrix a = Matrix [[a]]

instance Show a => Show (Matrix a) where
    show (Matrix xx) | null xx = "EMPTY" 
                     | otherwise = showlines xx
      where showlines [] = ""
            showlines [x] = show x
            showlines (x:xs) = show x ++ "\n" ++ showlines xs