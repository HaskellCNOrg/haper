
type Name = String

data Term = Var
          | Con Int
          | Add Term Term
          | Lam Name Term
          | App Term Term

data Value = Wrong
           | Num Int
           | Fun (Value -> M Value)
             
type Environment = [(Name, Value)]

showval :: Value -> String
showval Wrong = "<wrong>"
showval (Num i) = showint i
showval (Fun f) = "<function>"

interp :: Term -> Environment -> M Value
interp (Var x) e = lookup x e
interp (Con i) e = unitM (Num i)
interp (Add u v) e = interp u e `bindM` (\a ->
                     interp v e `bindM` (\b ->
                     add a b))
interp (Lam x v) e = unitM (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e = interp t e `bindM` (\f ->
                     interp u e `bindM` (\a ->
                     apply f a))

lookup :: Name -> Environment -> M Value
lookup x [] = unitM
lookup x ((y,b):e)  = if x==y then unitM b else lookup x e

add :: Value -> Value -> M Value
add (Num i) (Num j) = unitM (Num (i+j))
add a b             = unitm Wrong

apply :: Value -> Value -> M Value
apply (Fun k) a   = k a
apply f a         = unitM Wrong

test  :: Term -> String
test t = showM (interp t [])
