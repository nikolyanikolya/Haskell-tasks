
-- "Окружите" каждый элемент списка заданными "скобками", используя монаду списка и do-нотацию:
surround :: a -> a -> [a] -> [a]
surround x y zs = do 
    z <- zs
    [x, z, y]  