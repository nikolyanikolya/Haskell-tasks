--Реализуйте оператор (!!!) доступа к элементам массива по индексу, 
--отличающийся от стандартного (!!) 
--поведением в исключительных ситуациях. В этих ситуациях он должен 
--выбрасывать подходящее исключение типа ListIndexError.
{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.Except
import Control.Monad.Trans.Except (throwE)

data ListIndexError = 
  ErrTooLargeIndex Int 
  | ErrNegativeIndex 
  | OtherErr String
  deriving (Eq, Show)

infixl 9 !!!

(!!!) :: MonadError ListIndexError m => [a] -> Int -> m a

xs !!! n | n < 0 = throwError ErrNegativeIndex
         | length(take n xs)==length(take (n+1) xs) = throwError $ ErrTooLargeIndex n 
         | otherwise = return $ xs !! n 