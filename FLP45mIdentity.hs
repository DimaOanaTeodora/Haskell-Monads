--- Monada Identity -- Interpretorul monadic general

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

type M = Identity

type Name = String

data Term = Var Name
          | Con Integer
          | Term :+: Term
          | Lam Name Term
          | App Term Term
  deriving (Show)

data Value = Num Integer
           | Fun (Value -> M Value) -- Value -> Identity Value
           | Wrong

instance Show Value where 
 show (Num x) = show x
 show (Fun _) = "<function>"
 show Wrong   = "<wrong>"

type Environment = [(Name, Value)] --[(String, Value)]

showM :: Show a => M a -> String
showM ma = show (runIdentity ma)

interp :: Term -> [(Name, Value)] -> M Value
-- Term -> [(String, Value)] ->  Identity Value
interp (Var string) env = aux (lookup string env)
  where
    aux (Just value) = return value -- Identity Value
    aux Nothing = return Wrong
interp (Con n) env = return (Num n)
interp (t1 :+: t2) env = do
  x <- interp t1 env -- x de tipul Value
  y <- interp t2 env -- y de tipul Value
  auxsum x y
  where
    auxsum (Num n1)(Num n2) = return (Num (n1+n2))
    auxsum _ _ = return Wrong
interp (Lam string t) env = return $ Fun $ \v -> interp t ((string,v):env)
interp (App t1 t2) env = do
  x <- interp t1 env
  y <- interp t2 env
  auxapp x y 
  where
      auxapp (Fun f) value = f value -- Fun (Value -> M Value)
      auxapp _ _ = return Wrong

test :: Term -> String
test t = showM $ interp t []

term0:: Term
term0 = App (Lam "x" ((Var "x") :+: (Var "x"))) ((Con 10) :+:  (Con 11))
-- test term0 => "42"
