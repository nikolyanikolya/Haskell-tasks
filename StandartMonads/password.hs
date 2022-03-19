--Используя пользовательский тип ошибки 
--и трансформер ExceptT вместо MaybeT, 
--модифицируйте приведенный выше код так, 
--чтобы он выдавал пользователю 
--сообщение о причине, по которой пароль отвергнут.
{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (msum)
import Data.Char (isNumber, isPunctuation)
import Control.Monad.Except

newtype PwdError = PwdError String

type PwdErrorIOMonad = ExceptT PwdError IO

askPassword :: PwdErrorIOMonad ()
askPassword = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Storing in database..."

instance Semigroup PwdError where
  PwdError x <> PwdError y =  PwdError (x++y)

instance Monoid PwdError where
  mempty = PwdError ""
getValidPassword :: PwdErrorIOMonad String

getValidPassword = request `catchError` handler 
request = do
        password <- liftIO getLine
        isValid password
        return password
handler (PwdError e) = do 
        liftIO (putStrLn e)
        throwError (PwdError e)        
isValid s | length s < 8 = throwError (PwdError "Incorrect input: password is too short!")
          | not (any isNumber s) = throwError (PwdError "Incorrect input: password must contain some digits!")
          | not (any isPunctuation s) = throwError (PwdError "Incorrect input: password must contain some punctuations!")
          | otherwise = return s