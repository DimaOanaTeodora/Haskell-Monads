--- Monada Lista

--- Limbajul si  Interpretorul

type Name = String

data Term = Var Name
          | Con Integer
          | Term :+: Term
          | Lam Name Term
          | App Term Term
          | Fail 
          | Ned [Term] -- expresie formata din oricate valori vrem
          | Amb Term Term
  deriving (Show)

data Value = Num Integer
           | Fun (Value -> M Value)
           | Wrong

instance Show Value where
 show (Num x) = show x
 show (Fun _) = "<function>"
 show Wrong   = "<wrong>"

type Environment = [(Name, Value)] -- [(String, Value)]

type M a = [a]-- nu mai trebuie scrisa pt ca e deja monada

{-- nu mai am nevoie de el
showM :: Show a => M a -> String
showM (Just value) = show value
showM Nothing = error "Error"
-}

interp :: Term -> Environment -> M Value -- Term -> [(String, Value)] -> Either Value
interp Fail _ = []
interp (Con n) env = return (Num n)
interp (Var string) env =  aux (lookup string env )
-- (lookup string env) => Maybe Value : Num Integer, Fun, Wrong..
      where
            aux (Just value) = return value
            aux Nothing = return Wrong
interp (Amb t1 t2) env = (interp t1 env) ++ (interp t2 env) 
interp (Ned xs) env = concat [interp t env| t<- xs]
interp (t1 :+: t2) env = do
       x <- interp t1 env
       y <- interp t2 env
       add x y
       where 
             add (Num x) (Num y) = return (Num (x+y))
             add _ _ = return Wrong

interp (Lam string t) env = return $ Fun $ \v -> interp t ((string,v):env)
interp (App t1 t2) env = do
       f <- interp t1 env
       x <- interp t2 env
       app f x 
       where 
             app (Fun func) x = func x
             app _ _ = return Wrong

-- !! NU mai am nevoie de showM !!
term0 = (App (Lam "x" (Var "x" :+: Var "x")) (Amb (Con 1) (Con 2))) 
-- interp term0 [] => [2,4]
