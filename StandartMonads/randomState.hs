--Вычислите усреднённый по 
--k сериям модуль отклонения количества орлов от своего 
--теоретического среднего значения в серии. 
--Предполагается, что монетка "честная", то есть теоретическое 
--среднее известно и равно половине длины серии nn.

--Генератор случайных чисел подразумевается созданным с помощью 
--mkStdGen и передается в процессе вычислений 
--через монаду State. Используйте реализованную на практике
-- функцию randomRState
import System.Random
import Control.Monad.State

import Control.Monad.Writer
import Data.IORef
import qualified Control.Monad

res :: [[Int]] -> Int -> Int -> Double
res xs k n = foldr (\ x -> (+) (abs $ fromIntegral (sum x) - fromIntegral n / 2)) 0.0 xs

--randomR :: RandomGen g => (a, a) -> g -> (a, g)
randomRState :: (Random a, RandomGen g) => (a, a) -> State g a
randomRState (x, y) = do
    h <- get
    let rand_pair = randomR (x, y) h
    put (snd rand_pair)
    return (fst rand_pair)

avgdev' :: Int -> Int -> State StdGen Double
avgdev' k n = do
        xs <- replicateM k (replicateM n $ randomRState (0, 1))
        return (res xs k n / fromIntegral k)