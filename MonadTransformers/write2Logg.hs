import Control.Monad
import Control.Monad.Identity
data Logged a = Logged String a deriving (Eq,Show)

newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }

instance Monad m => Functor (LoggT m) where
  fmap = liftM

instance Monad m => Applicative (LoggT m) where
  pure = return
  (<*>) = ap
  
instance Monad m => Monad (LoggT m) where
  return  = LoggT . return . Logged mempty 
  m >>= k  = LoggT $ do
      Logged v a <- runLoggT m
      Logged u b <- runLoggT (k a)
      return (Logged (u <> v) b)
instance MonadFail m => MonadFail (LoggT m) where
  fail = LoggT . fail

write2log :: Monad m => String -> LoggT m ()
write2log s = LoggT $ return $ Logged s () 

type Logg = LoggT Identity
    
runLogg :: Logg a -> Logged a
runLogg = runIdentity . runLoggT