--- Monada EnvReader
-- contine mediul de evaluare => dispare ca parametru
-- functia interp are doar 2 parametrii
-- o noua functie pentru lookup
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
                g env = (runReader ma) (f env)  
                -- pastrez actiunea functiei asupra mediului modificat prin f

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
showM ma = show $ runReader ma [] 

-- Contine deja mediul de evaluare
interp :: Term -> M  Value -- NOU
-- Term -> EnvReader Value
interp (Con n)  = return (Num n)
interp (Var string) = do -- NOU
                  env <- ask -- iau mediul de evaluare (care este constant)
                  case lookup string env of     
                    Just val -> return val     
                    Nothing -> return Wrong

interp (t1 :+: t2) = do
       x <- interp t1 --NOU : fara mediu de evaluare
       y <- interp t2 
       auxsum x y
       where
        auxsum (Num n1)(Num n2) = return (Num (n1+n2))
        auxsum _ _ = return Wrong 

interp (Lam string t) = do
                        env <- ask -- NOU
                        return $ Fun  (\v -> let f = \env -> ((string,v): env) in (local f ( interp t)))
                        -- Fun :: Value -> M Value
      
interp (App t1 t2)  = do
       f <- interp t1 --NOU : fara mediu de evaluare
       x <- interp t2 
       auxapp f x 
       where
        auxapp (Fun f) value = f value -- Fun (Value -> M Value)
        auxapp _ _ = return Wrong

test :: Term -> String
test t = showM $ interp t -- NOU : dispare mediul de evaluare

term0:: Term
term0 = App (Lam "x" ((Var "x") :+: (Var "x"))) ((Con 10) :+:  (Con 11))
-- test term0 => "42"
