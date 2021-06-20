
--- Monada Maybe

--- Limbajul si  Interpretorul

type Name = String

data Term = Var Name
          | Con Integer
          | Term :+: Term
          | Lam Name Term
          | App Term Term
  deriving (Show)

data Value = Num Integer
           | Fun (Value -> M Value)
           | Wrong

instance Show Value where
 show (Num x) = show x
 show (Fun _) = "<function>"
 show Wrong   = "<wrong>"

type Environment = [(Name, Value)] -- [(String, Value)]

type M = Maybe -- nu mai trebuie scrisa pt ca e deja monada

{-- nu mai am nevoie de el
showM :: Show a => M a -> String
showM (Just value) = show value
showM Nothing = error "Error"
-}

interp :: Term -> Environment -> M Value -- Term -> [(String, Value)] -> Identity Value
interp (Con n) env = return (Num n)
interp (Var string) env = lookup string env -- intoarce un Maybe Value si e ok aici
interp (t1 :+: t2) env = do
       x <- interp t1 env
       y <- interp t2 env
       return (add x y)
       where 
             add (Num x) (Num y) = Num (x+y)
             add _ _ = Wrong  

interp (Lam string t) env = return $ Fun $ \v -> interp t ((string,v):env)
interp (App t1 t2) env = do
       f <- interp t1 env
       x <- interp t2 env
       app f x 
       where 
             app (Fun func) x = func x
             app _ _ = return Wrong 

-- !! NU mai am nevoie de showM !!
term0 = (App (Lam "x" (Var "x" :+: Var "x")) (Con 10 :+: Con 11))
-- interp term0 [] => "Just 42"

pgm1 = App
          (Lam "x" ((Var "x") :+: (Var "x")))
          ((Con 10) :+:  (Con 11))
-- interp pgm1 [] => "Just 7" 
