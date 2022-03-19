--Реализуйте функции, осуществляющие подстановку 
--типов вместо переменных 
--типа в тип (appSubsTy) и подстановку 
--типов вместо переменных типа в контекст (appSubsEnv):
import qualified Data.Bifunctor
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

appSubsTy :: SubsTy -> Type -> Type
appSubsTy (SubsTy sbs) (TVar v) = case lookup v sbs of
  Just t -> t
  Nothing -> TVar v
appSubsTy (SubsTy sbs) (x :-> y) = appSubsTy (SubsTy sbs) x :-> appSubsTy (SubsTy sbs) y
appSubsEnv :: SubsTy -> Env -> Env
appSubsEnv sbs (Env env) = Env $ map (Data.Bifunctor.second (appSubsTy sbs)) env 