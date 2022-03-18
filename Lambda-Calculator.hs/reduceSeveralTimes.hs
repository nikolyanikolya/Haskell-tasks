--Реализуйте функцию многошаговой β-редукции к нормальной форме, использующую нормальную стратегию:
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List
import Control.Arrow ()
import Data.Maybe
type Symb = String 
infixl 2 :@
infix 1 `alphaEq`

data Expr = Var Symb
          | Expr :@ Expr
          | Lam Symb Expr
          deriving (Eq, Read, Show)

removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                              | otherwise = y : removeItem x ys

allVars:: Expr -> [Symb]
allVars (Var x) = [x]
allVars (x :@ y) = allVars x `union` allVars y
allVars (Lam x y) = [x] `union` allVars y
freeVars :: Expr -> [Symb]
freeVars (Var x) = [x]
freeVars (x :@ y) = freeVars x `union` freeVars y
freeVars (Lam x y) = removeItem x (freeVars y)

makeNew:: Symb -> [Symb] -> [Symb] -> Symb
makeNew n free1 free2 | n `elem` free1 || n `elem` free2 = makeNew (n++['1']) free1 free2
                      | otherwise = n
subst :: Symb -> Expr -> Expr -> Expr
subst v n m = subst' v n m (freeVars n)
subst' :: Symb -> Expr -> Expr -> [Symb] -> Expr
subst' v n (Var x) free1  | v == x  = n
                          | otherwise = Var x
subst' v n (x :@ y) free1 = subst' v n x free1 :@ subst' v n y free1
subst' v n (Lam x e) free1  | x == v = Lam x e
                            | x `elem` free1 = Lam newEl (subst' v n (subst' x (Var newEl) e free1 ) free1)
                            | otherwise = Lam x (subst' v n e free1)
                            where newEl = makeNew x free1 (freeVars e)
alphaEq :: Expr -> Expr -> Bool
alphaEq (Var x) (Var y) | x == y = True
                        | otherwise = False
alphaEq (x :@ y) (l :@ r) = alphaEq x l && alphaEq y r
alphaEq (Lam x e1) (Lam y e2) |  x == y = alphaEq e1 e2 
                              | otherwise = alphaEq e2 (subst x (Var y) e1) && notElem y (freeVars e1)
alphaEq _ _ = False

reduceOnce :: Expr -> Maybe Expr
reduceOnce (Var x) = Nothing
reduceOnce ((Lam x e1) :@ e2) = Just $ subst x e2 e1
reduceOnce (Lam x e)  = case reduceOnce e of
    Nothing -> Nothing
    Just t -> Just $ Lam x t
reduceOnce (l :@ r) = case reduceOnce l of
    Just t -> Just $ t :@ r
    Nothing -> case reduceOnce r of
        Nothing -> Nothing
        Just t -> Just $ l :@ t
nf :: Expr -> Expr 
nf e = nf' e (Just e)
nf' :: Expr -> Maybe Expr -> Expr
nf' e prev =  case reduceOnce e of
    Just reduce -> nf' reduce (Just reduce)
    Nothing -> fromJust prev