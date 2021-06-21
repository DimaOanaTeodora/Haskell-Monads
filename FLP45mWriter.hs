--- Monada String Writer - afisare rezultate intermediare

newtype StringWriter a = StringWriter { runStringWriter :: (a, String) }

instance  Monad StringWriter where
  return va = StringWriter (va, "")
  ma >>= k = let (va, log1) = runStringWriter ma
                 (vb, log2) = runStringWriter (k va)
             in  StringWriter (vb, log1 ++ log2)


instance  Applicative StringWriter where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor StringWriter where              
  fmap f ma = pure f <*> ma 


--- Limbajul si  Interpretorul

type M = StringWriter 

type Name = String

data Term = Var Name
          | Con Integer
          | Out Term -- adaug aici
          | Term :+: Term
          | Lam Name Term
          | App Term Term
  deriving (Show)

data Value = Num Integer
           | Fun (Value -> M Value) 
           | Wrong
      
tell:: String -> StringWriter () 
tell mesaj = StringWriter ((), mesaj)

instance Show Value where 
 show (Num x) = show x
 show (Fun _) = "<function>"
 show Wrong   = "<wrong>"

type Environment = [(Name, Value)]

interp :: Term -> [(Name, Value)] -> M Value 
-- Term -> [(String, Value)] -> StringWriter Value
interp (Var name) env = aux name env 
  where 
    aux name env = case lookup name env of     
              Just val -> return val     
              Nothing ->  return Wrong
interp (Con const) _ = do
                      tell(show const ++ ";")
                      return (Num const)
interp (Out t) env = do
                      tell("Output: ")
                      interp t env 
interp (x1 :+: x2) env = do
                          -- le scoate din cutie de aceea v1 si v2 sunt de tip Value
                          v1 <- interp x1 env
                          v2 <- interp x2 env
                          return (auxsum v1 v2)
                          where 
                            auxsum (Num v1) (Num v2) = Num (v1+v2)
                            auxsum _ _ = Wrong
                          
interp (Lam name exp) env = return $ Fun $ \v -> interp exp ((name,v):env)
interp (App x1 x2) env = do
                          f <- interp x1 env
                          val <- interp x2 env
                          apply f val
                          where
                            apply (Fun k) v = k v
                            apply _ _ = return Wrong

showM :: Show a => M a -> String
showM ma = show (runStringWriter ma) 

test :: Term -> String
test t = showM $ interp t []

pgm1:: Term
pgm1 = (Out (Con 41) :+: Out (Con 1)) => "(42,\"Output: 41;Output: 1;\")"
