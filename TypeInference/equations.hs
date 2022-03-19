--Реализуйте алгоритм построения системы уравнений 
--на типы для заданных контекста,
--терма и инициализирующего типа для терма
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import qualified Data.Bifunctor
import Data.List
import Control.Monad.Error
import Control.Monad.State.Class
import Control.Monad.State.Lazy (StateT)
import Control.Monad.Trans.State.Lazy (evalStateT)
import Data.Bifunctor
infixl 4 :@
infixr 3 :->

type Symb = String

-- Терм
data Expr = Var Symb
          | Expr :@ Expr
          | Lam Symb Expr
  deriving (Eq,Show)

-- Тип
data Type = TVar Symb
          | Type :-> Type
  deriving (Eq,Show)

-- Контекст
newtype Env = Env [(Symb,Type)]
  deriving (Eq,Show)

-- Подстановка
newtype SubsTy = SubsTy [(Symb, Type)]
  deriving (Eq,Show)

removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                      | otherwise = y : removeItem x ys
freeVars :: Expr -> [Symb]
freeVars (Var x) = [x]
freeVars (x :@ y) = freeVars x `union` freeVars y
freeVars (Lam x y) = removeItem x (freeVars y)

freeTVars :: Type -> [Symb]
freeTVars (TVar x) = [x]
freeTVars (a :-> b) = freeTVars a `union` freeTVars b

extendEnv :: Env -> Symb -> Type -> Env
extendEnv (Env e) s t = case lookup s e of
    Just _ -> Env e
    Nothing -> Env $ (s, t) : e

freeTVarsEnv :: Env -> [Symb]
freeTVarsEnv (Env env)= nub $ concatMap (\(_, t) -> freeTVars t) env
appSubsTy :: SubsTy -> Type -> Type
appSubsTy (SubsTy sbs) (TVar v) = case lookup v sbs of
  Just t -> t
  Nothing -> TVar v
appSubsTy (SubsTy sbs) (x :-> y) = appSubsTy (SubsTy sbs) x :-> appSubsTy (SubsTy sbs) y
appSubsEnv :: SubsTy -> Env -> Env
appSubsEnv sbs (Env env) = Env $ map (Data.Bifunctor.second (appSubsTy sbs)) env
appEnv :: MonadError String m => Env -> Symb -> m Type
appEnv (Env xs) v = case lookup v xs of
  Just x -> return x
  Nothing -> throwError ("There is no variable " ++ show v ++ " in the environment.")
composeSubsTy :: SubsTy -> SubsTy -> SubsTy
composeSubsTy left@(SubsTy s1) right@(SubsTy s2) = let
  carrier pairs = map fst pairs
  result = carrier s1 `union` carrier s2
  in SubsTy $ map (\symb -> (symb, appSubsTy left (appSubsTy right (TVar symb)))) result

instance Semigroup SubsTy where
  (<>) = composeSubsTy
instance Monoid SubsTy where
  mempty = SubsTy []

is_in _ [] = False
is_in x (y : ys) = (x == y) || is_in x ys


unify :: MonadError String m => Type -> Type -> m SubsTy
unify (TVar x) (TVar y)  | x == y =  return (SubsTy [])
                         | otherwise = return (SubsTy [(x, TVar y)])
unify (TVar x) y  | is_in x (freeTVars y) = throwError ("Can't unify (" ++ show (TVar x) ++ ") with (" ++ show y ++")!")
                  | otherwise = return (SubsTy [(x, y)])
unify (x :-> y) (TVar z) = unify (TVar z) (x :-> y)
unify (x:-> y) (a :-> b) = do
   u2 <- unify y b
   u <- unify (appSubsTy u2 x) (appSubsTy u2 a)
   return $ composeSubsTy u u2       

getFreshTVar:: MonadState Integer m => m Type   
getFreshTVar = do
  c <- get
  put (succ c)
  return (TVar ("a" ++ show c))   
getList :: MonadState Integer m => Int -> m [Type]
getList 0 = return []
getList n = do
         xs <- getList (n - 1)
         var <- getFreshTVar
         return (var : xs)    
equations :: MonadError String m => Env -> Expr -> Type -> m [(Type,Type)]
equations env var tt = evalStateT (equations' env var tt) 1      
equations' :: MonadError String m => Env -> Expr -> Type -> StateT Integer m [(Type,Type)]
equations' (Env env) (Var x) y = do
  t <- appEnv (Env env) x
  return [(y, t)] 
equations' (Env env) (a :@ b) y = do
  var <- getFreshTVar
  l <- equations' (Env env) b var
  k <- equations' (Env env) a (var :-> y)
  return (k `union` l)
equations' (Env env) (Lam x n) y = do
  var1 <- getFreshTVar
  var2 <- getFreshTVar
  k <- equations' (extendEnv (Env env) x var1) n var2
  let l = [(var1 :-> var2, y)]
  return (k `union` l)
principlePair :: MonadError String m =>  Expr -> m (Env,Type)
principlePair e = evalStateT (principlePair' e) 1

pairs [] = (TVar "", TVar "")
pairs [(x, y)] = (x, y)
pairs ((x, y) : xs) = bimap (x :->) (y :->) (pairs xs)

principlePair' :: MonadError String m =>  Expr -> StateT Integer m (Env,Type)
principlePair' e =  do
  let free_variables = freeVars e 
  let start_var = TVar "r"
  list <- getList (4 * length free_variables)
  let env = Env $ zip free_variables list
  l <- equations env e start_var
  let left = fst $ pairs l
  let right = snd $ pairs l
  str <- unify left right
  return $ (,) (appSubsEnv str env) (appSubsTy str start_var)  
  

