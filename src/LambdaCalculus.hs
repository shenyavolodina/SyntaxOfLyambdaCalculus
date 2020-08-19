module LambdaCalculus where
import Data.List

-- 1
data Term = Var String | Abs String (Term) | App (Term) (Term) 
    deriving (Eq, Show)

    
-- 2
allVars :: Term -> [String]
allVars (Var x) = [x]
allVars (Abs x t) = allVars t ++ [x]
allVars (App t1 t2) = allVars t1 ++ allVars t2

isFresh :: Term -> String -> Bool
isFresh (term) x = notElem x (allVars term)

-- 3
freeVars :: Term -> [String]
freeVars (Var x) = [x]
freeVars (Abs x t) = freeVars t \\ [x]
freeVars (App t1 t2) = freeVars t1 ++ freeVars t2

isFree :: Term -> String -> Bool
isFree (term) x = elem x (freeVars term)

-- 4
isSubterm :: Term -> Term -> Bool
isSubterm (term) (subterm) | term == subterm = True
isSubterm (Var x) (subterm) = Var x == subterm
isSubterm (Abs _ t) (subterm) = isSubterm t subterm
isSubterm (App t1 t2) (subterm) = isSubterm t1 subterm || isSubterm t2 subterm

-- 5
isBound :: Term -> String -> Bool
isBound (Var _) (_) = False
isBound (Abs x t) varName = (varName == x && _isBound t varName) || isBound t varName
isBound (App t1 t2) varName = isBound t1 varName || isBound t2 varName

_isBound :: Term -> String -> Bool
_isBound (Var x) varName = x == varName
_isBound (App t1 t2) varName = _isBound t1 varName || _isBound t2 varName
_isBound _ _ = False



