--- Monada EnvReader

newtype EnvReader a = Reader { runReader :: Environment -> a}

instance Monad EnvReader where 
  return va = Reader (\ _ -> va) 
  ma >>= k =  Reader g 
              where 
                g env = let va = runReader ma env in runReader (k va) env

instance Applicative EnvReader where
  pure = return
  mf <*> ma = do
              f <-mf
              a <-ma
              return (f a)

instance Functor EnvReader where
  fmap f ma = pure f <*> ma

---- !! Am nevoie de aceste functii auxiliare ----
ask :: EnvReader Environment
ask = Reader id

-- ne da voie sa schimbam mediul de evaluare
local :: (Environment -> Environment) -> EnvReader a -> EnvReader a
local f ma =  Reader g
              where
                g env = (runReader ma) (f env)  -- pastrez actiunea functiei asupra mediului modificat prin f

--- Limbajul si  Interpretorul

type M = EnvReader

type Name = String

data Term = Var Name
          | Con Integer
          | Term :+: Term
          | Lam Name Term
          | App Term Term
  deriving (Show)

data Value = Num Integer
           | Fun (Value -> M Value) -- M de tipul Identity
           | Wrong

instance Show Value where -- ma uit la data Value
 show (Num x) = show x
 show (Fun _) = "<function>"
 show Wrong   = "<wrong>"

type Environment = [(Name, Value)]

showM :: Show a => M a -> String
showM ma = show $ runReader ma [] -- trebuie pus [] de la mediul de evaluare

-- contine deja mediul de evaluare
interp :: Term -> M  Value -- Term -> EnvReader Value
interp (Con n)  = return (Num n)
interp (Var string) = lookupAux string
-- (lookup string env) => Maybe Value : Num Integer, Fun, Wrong..
      where
            lookupAux strin = do
                  env <- ask
                  case lookup string env of     
                    Just val -> return val     
                    Nothing -> return Wrong

interp (t1 :+: t2) = do
      -- (interp t1 env) => Identity Value, ci NU un Value simplu => am nevoie de do
       x <- interp t1 
       y <- interp t2 
       return (add x y) -- return il baga in Identity si add aduna doua valori de tipul Value si intoarce tot un Value
       where 
             add (Num x) (Num y) = Num (x+y)
             add _ _ = Wrong  

interp (Lam string t) = do
                        env <- ask -- iau env cu ask
                        return $ Fun  (\v -> let f = \env -> ((string,v): env) in (local f ( interp t)))
                      -- Fun :: Value -> M Value
      
interp (App t1 t2)  = do
-- (interp t1 env) => Identity Value, ci NU un Value simplu => am nevoie de do
       f <- interp t1 
       x <- interp t2 
       app f x 
       where 
             app (Fun func) x = func x
             app _ _ = return Wrong  -- nu mai dau return sus

pgm1:: Term
pgm1 = App
          (Lam "x" ((Var "x") :+: (Var "x")))
          ((Con 10) :+:  (Con 11))
-- showM $ interp pgm1 => "42"
