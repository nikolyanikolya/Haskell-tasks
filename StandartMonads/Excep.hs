{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.Except
import GHC.Base (Alternative)
import Control.Applicative (empty, (<|>))

data Excep a =  Err String | Ok a 
  deriving (Eq, Show)

instance Functor Excep where
    fmap f (Ok a) = Ok $ f a
    fmap f (Err x) = Err x

instance Applicative Excep where
    pure = return
    (<*>) = ap
--Реализуйте собственную монаду обработки ошибок (взамен Either e) 
--со строковым типом информации об ошибке на основе типа данных
instance Monad Excep where
    return = Ok
    (Ok a) >>= k = k a
    (Err x) >>= k = Err x
instance MonadPlus Excep where
    mzero = Err "MonadPlus.mzero error."
    mplus = (<|>)
instance Alternative Excep where
    empty = Err "Alternative.empty error."
    (Ok a) <|> _ = Ok a
    (Err x) <|> z = z
instance MonadFail Excep where
    fail a = Err "Monad.fail error."
instance (MonadError String) Excep where
    throwError = Err
    (Err a) `catchError` k =  k a
    m `catchError` k = m
-- тестирование
(?/) :: (MonadError String m) 
            => Double -> Double -> m Double
x ?/ 0 = throwError "Division by 0."
x ?/ y = return $ x / y

example :: Double -> Double -> Excep String
example x y = action  `catchError` return where 
  action = do 
    q <- x ?/ y
    guard (q >=0)
    if q  > 100 then do 
      100 <- return q
      undefined
    else 
      return $ show q
    