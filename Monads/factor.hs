--Разложите положительное целое число на два сомножителя всевозможными способами, используя монаду списка и do-нотацию:
isfactor x n = n `mod` x == 0
factor2 :: Integer -> [(Integer, Integer)]
factor2 n = do 
    let y = toInteger $ floor $ sqrt $ fromInteger n
    x <- [1..y]
    True <- return $ isfactor x n
    [(,) x (n `div` x)]