--Реализуйте следующий набор вспомогательных функций. 
--Функция freeVars возвращает список свободных переменных терма
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