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
          | Out Term -- NOU: afisare valoare + ";"
          | Term :+: Term
          | Lam Name Term
          | App Term Term
  deriving (Show)

data Value = Num Integer
           | Fun (Value -> M Value) 
           | Wrong
      
tell:: String -> StringWriter ()  -- NOU: afiseaza valoarea data ca argument
tell mesaj = StringWriter ((), mesaj)

instance Show Value where 
 show (Num x) = show x
 show (Fun _) = "<function>"
 show Wrong   = "<wrong>"

type Environment = [(Name, Value)]

interp :: Term -> [(Name, Value)] -> M Value 
-- Term -> [(String, Value)] -> StringWriter Value
interp (Var name) env = aux (lookup name env)
  where 
    aux (Just val) = return val     
    aux Nothing = return Wrong
interp (Con n) env = return (Num n)
interp (Out t) env = do 
                      value <- interp t env 
                      tell (show value ++ "; ") -- NOU
                      return value 
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

{-
Daca vreau ca instanta a clasei show:
instance (Show a) => Show (StringWriter a) where
  show(StringWriter (val, env))
    | null env  = "Value: " ++ show val
    | otherwise = "Output: " ++ env ++ "Value: " ++ show val
-}

showM :: Show a => M a -> String
showM (StringWriter (val, env))
    | null env  = "Value: " ++ show val
    | otherwise = "Output: " ++ env ++ "Value: " ++ show val

test :: Term -> String
test t = showM $ interp t []

term0 :: Term
term0 = (Out (Con 41) :+: Out (Con 1)) 
-- test term0 => "Output: 41; 1; Value: 42"
