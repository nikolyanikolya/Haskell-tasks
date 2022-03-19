--Чтобы избавится от необходимости ручного подъема операции write2log, 
--обеспечивающей стандартный интерфейс 
--вложенного трансформера LoggT, можно поступить 
--по аналогии с другими трансформерами 
--библиотеки mtl. А именно, разработать класс 
--типов MonadLogg, выставляющий этот стандартный интерфейс
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Reader
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

logSt :: LoggT (State Integer) Integer
logSt = do
  lift $ modify (+1)
  a <- lift get
  write2log $ show $ a * 10
  lift $ put 42
  return $ a * 100
runLogg :: Logg a -> Logged a
runLogg = runIdentity . runLoggT

instance MonadTrans LoggT where
  lift = LoggT . fmap (Logged mempty)

instance MonadState s m => MonadState s (LoggT m) where
    get  =  LoggT (gets (Logged mempty))
    put   = (LoggT . fmap (Logged mempty)) . put
    state = (LoggT . fmap (Logged mempty)) . state
instance MonadReader r m => MonadReader r (LoggT m) where
    ask    = lift ask
    local  = mapLoggT . local
    reader = lift . reader
mapLoggT :: (m (Logged a) -> n (Logged b)) -> LoggT m a -> LoggT n b
mapLoggT f (LoggT m) = LoggT $ f m  

class Monad m => MonadLogg m where
  w2log :: String -> m ()
  logg :: Logged a -> m a

instance Monad m => MonadLogg (LoggT m) where
  w2log = write2log
  logg =  LoggT . return   

instance MonadLogg m => MonadLogg (StateT s m) where
  w2log = lift . w2log 
  logg  = lift . logg 

instance MonadLogg m => MonadLogg (ReaderT r m) where
  w2log = lift . w2log
  logg  = lift . logg