--- Monada Identity

newtype Identity a = Identity { runIdentity :: a }

instance Monad Identity where
  return va = Identity va 
  ma >>= k =  k (runIdentity ma) 

instance Applicative Identity where
  pure = return
  mf <*> ma = do
              f <-mf
              a <-ma
              return (f a)

instance Functor Identity where
  fmap f ma = pure f <*> ma

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

type M = Identity

showM :: Show a => M a -> String
showM ma = show (runIdentity ma)

interp :: Term -> Environment -> M Value
-- Term -> [(String, Value)] -> Identity Value
interp (Con n) env = return (Num n)
interp (Var string) env = aux (lookup string env )
-- (lookup string env) => Maybe Value : Num Integer, Fun, Wrong..
      where
            aux (Just value) = return value
            aux Nothing = return Wrong
interp (t1 :+: t2) env = do
      -- (interp t1 env) => Identity Value, ci NU un Value simplu => am nevoie de do
       x <- interp t1 env
       y <- interp t2 env
       return (add x y) -- return il baga in Identity si add aduna doua valori de tipul Value si intoarce tot un Value
       where 
             add (Num x) (Num y) = Num (x+y)
             add _ _ = Wrong  

interp (Lam string t) env = return $ Fun $ \v -> interp t ((string,v):env)
interp (App t1 t2) env = do
-- (interp t1 env) => Identity Value, ci NU un Value simplu => am nevoie de do
       f <- interp t1 env
       x <- interp t2 env
       app f x 
       where 
             app (Fun func) x = func x
             -- (Fun f)este de forma (Value -> M Value)
             -- nu trebuie return ca fun transforma el automat in M Value
             app _ _ = return Wrong  -- nu mai dau return sus

term0 = (App (Lam "x" (Var "x" :+: Var "x")) (Con 10 :+: Con 11))
-- showM $ interp term0 [] => "42"
