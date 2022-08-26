{-
Реализуйте алгоритм подстановки терма nn вместо всех свободных 
вхождений переменной v в терме m (в стандартной нотации m[v:=n])
    -}
    {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
    import Data.List
    
    type Symb = String
    infixl 2 :@
    
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
    