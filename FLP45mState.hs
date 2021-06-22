--- Monada State
newtype InState  a = State {runState :: Integer -> (a, Integer)}

instance Monad InState where
  return va = State ( \s -> (va,s))
  ma >>= k = State g
              where
                g s = let (va, news)=(runState ma s ) in (runState (k va) news) 

instance Applicative InState where
  pure = return
  mf <*> ma = do
              f <-mf
              a <-ma
              return (f a)

instance Functor InState where
  fmap f ma = pure f <*> ma

get :: InState Integer -- NOU
get = State ( \s -> (s,s)) 
-- functie asemanatoare cu ask
-- folosit la numararea operatiilor prin Count ca altfel ramane 0

modify :: (Integer -> Integer) -> InState () -- NOU
modify f = State (\s -> ((), f s)) -- primeste o stare si schimba starea prin aplicarea lui f

--- Limbajul si  Interpretorul

type M = InState

type Name = String

data Term = Var Name
          | Con Integer
          | Term :+: Term
          | Lam Name Term
          | App Term Term
          | Count -- NOU: numara operatiile
  deriving (Show)


data Value = Num Integer
           | Fun (Value -> M Value)
           | Wrong

instance Show Value where
 show (Num x) = show x
 show (Fun _) = "<function>"
 show Wrong   = "<wrong>"

type Environment = [(Name, Value)]

interp :: Term -> Environment -> M Value -- Term -> [(String, Value)] -> InState Value
interp (Var name) env = aux (lookup name env)
  where 
    aux (Just val) = return val     
    aux Nothing = return Wrong
interp (Con n) env = return (Num n)
interp Count env = do -- NOU: count nu depinde de mediu (env) pt ca el e starea 
                  s <- get -- iau starea
                  return (Num s) 
interp (t1 :+: t2) env = do
  x <- interp t1 env -- x de tipul Value
  y <- interp t2 env -- y de tipul Value
  auxsum x y
  where
    -- trebuie sa maresc contuarul, altfel e mereu 0
    auxsum (Num n1)(Num n2) = modify(+1) >>  return (Num (n1+n2)) --NOU
    auxsum _ _ = return Wrong
interp (Lam string t) env = return $ Fun $ \v -> interp t ((string,v):env)
interp (App t1 t2) env = do
  x <- interp t1 env
  y <- interp t2 env
  auxapp x y 
  where
      -- trebuie sa maresc contuarul, altfel e mereu 0
      auxapp (Fun f) value = modify(+1) >> f value -- NOU
      auxapp _ _ = return Wrong

showM :: Show a => M a -> String 
showM ma = let (va, s)=(runState ma 0 ) in ("Value: " ++ (show va) ++" Count: " ++ (show s)) --NOU

test :: Term -> String
test t = showM $ interp t []

term0 :: Term
term0 = App (Lam "x" ((Var "x") :+: (Var "x"))) ((Con 10) :+:  (Con 11))
-- test term0 => "Value: 42 Count: 3"
