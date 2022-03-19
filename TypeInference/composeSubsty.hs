--Реализуйте функцию, выполняющую композицию
--двух подстановок (носитель композиции 
--является объединением носителей двух этих подстановок):
import qualified Data.Bifunctor
import Data.List
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

composeSubsTy :: SubsTy -> SubsTy -> SubsTy
composeSubsTy left@(SubsTy s1) right@(SubsTy s2) = let
  carrier pairs = map fst pairs
  result = carrier s1 `union` carrier s2
  in SubsTy $ map (\symb -> (symb, appSubsTy left (appSubsTy right (TVar symb)))) result

instance Semigroup SubsTy where
  (<>) = composeSubsTy
instance Monoid SubsTy where
  mempty = SubsTy []