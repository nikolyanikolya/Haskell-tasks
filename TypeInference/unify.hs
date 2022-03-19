--Реализуйте алгоритм унификации, возвращающий 
--для двух переданных типов наиболее общий 
--унификатор или сообщение об ошибке, если унификация невозможна.
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import qualified Data.Bifunctor
import Data.List
import Control.Monad.Error
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