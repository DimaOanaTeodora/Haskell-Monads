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

get :: InState Integer --get intoarce starea ca valoare get:: State state state
get = State ( \s -> (s,s)) 
-- functie asemanatoare cu ask
-- folosit la numararea operatiilor prin Count ca altfel ramane 0

modify :: (Integer -> Integer) -> InState () 
modify f = State (\s -> ((), f s)) -- primeste o stare si schimba starea prin aplicarea lui f

--- Limbajul si  Interpretorul

type M = InState

type Name = String

data Term = Var Name
          | Con Integer
          | Term :+: Term
          | Lam Name Term
          | App Term Term
          | Count -- modific aici -> numara operatiile
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
interp (Var name) env = aux name env
            where
              aux name env = case lookup name env of     
                              Just val -> return val     
                              Nothing ->  return Wrong
interp (Con const) _ = return (Num const)
interp Count _ = do -- count nu depinde de mediu (env) pt ca el e starea 
                  s <- get -- iau starea
                  return (Num s) 
interp (x1 :+: x2) env = do 
                          v1 <- interp x1 env
                          v2 <- interp x2 env
                          auxsum v1 v2
                          where
                            --trebuie sa maresc count-ul ca altfel merge dar e mereu 0
                            auxsum (Num i) (Num j) = modify(+1) >>  return (Num (i + j)) 
                            auxsum _ _ = return Wrong

interp (Lam x exp) env = return $ Fun $ \v -> interp exp ((x,v):env)
interp (App x1 x2) env = do
                          f <- interp x1 env
                          val <- interp x2 env
                          apply f val
                          where
                            --si aici modific starea (count-ul) prin modify
                            apply (Fun k) v = modify(+1) >>  k v 
                            apply _ _ = return Wrong


showM :: Show a => M a -> String
showM ma = let (va, s)=(runState ma 0 ) in ("Value: " ++ (show va) ++" Count: " ++ (show s))

test :: Term -> String
test t = showM $ interp t []

pgm1:: Term
pgm1 = App
          (Lam "x" ((Var "x") :+: (Var "x")))
          ((Con 10) :+:  (Con 11))
--test pgm1 => "Value: 42 Count: 3"
