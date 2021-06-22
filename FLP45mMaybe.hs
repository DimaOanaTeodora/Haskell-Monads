--- Monada Maybe -- definita by default
-- Dispare showM pentru ca e deja definit si el
-- Nu mai apare Wrong in Value

--- Limbajul si  Interpretorul

type M = Maybe

type Name = String

data Term = Var Name
          | Con Integer
          | Term :+: Term
          | Lam Name Term
          | App Term Term
  deriving (Show)

data Value = Num Integer
           | Fun (Value -> M Value) -- Value -> Identity Value

instance Show Value where 
 show (Num x) = show x
 show (Fun _) = "<function>"

type Environment = [(Name, Value)] --[(String, Value)]

interp :: Term -> [(Name, Value)] -> M Value
-- Term -> [(String, Value)] ->  Maybe Value
interp (Var string) env = lookup string env -- returneaza deja un Maybe Value
interp (Con n) env = return (Num n)
interp (t1 :+: t2) env = do
  x <- interp t1 env -- x de tipul Value
  y <- interp t2 env -- y de tipul Value
  auxsum x y
  where
    auxsum (Num n1)(Num n2) = return (Num (n1+n2))
    auxsum _ _ = Nothing
interp (Lam string t) env = return $ Fun $ \v -> interp t ((string,v):env)
interp (App t1 t2) env = do
  x <- interp t1 env
  y <- interp t2 env
  auxapp x y 
  where
      auxapp (Fun f) value = f value -- Fun (Value -> M Value)
      auxapp _ _ = Nothing 

test :: Term -> String
test t = show $ interp t [] -- show-ul by default pt Maybe ca sa transforme in string

term0:: Term
term0 = App (Lam "x" ((Var "x") :+: (Var "x"))) ((Con 10) :+:  (Con 11))
-- test term0 => "Just 42"
-- interp t [] => Just 42
