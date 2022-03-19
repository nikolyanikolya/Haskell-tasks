--Напишите функцию

--while :: IORef a -> (a -> Bool) -> IO () -> IO ()
--while ref p action = do undefined
--позволяющую кодировать "императивные циклы" следующего вида:
import Data.IORef
import qualified Control.Monad
while :: IORef a -> (a -> Bool) -> IO () -> IO ()
while ref p action = do
    i <- readIORef ref
    Control.Monad.when (p i) $ do
       action
       while ref p action