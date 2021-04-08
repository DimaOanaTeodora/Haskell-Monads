
--- Monada Identity

newtype Identity a = Identity { runIdentity :: a } --ma = Identity va unde va :: a

instance Monad Identity where
  return va = Identity va --return = Identity (return pune valoarea in cutie)
  ma >>= k =  k (runIdentity ma)  -- k :: a -> mb

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

showM :: Show a => M a -> String
showM ma = show (runIdentity ma) -- in notatie functionala compunere: show .runIdentity
-- interp pgm [] => EROARE
-- showM $ interp pgm [] => "7"
-- showM $ interp pgm1 [] => "42"
-- vezi functia de testare mai jos
type Name = String

data Term = Var Name
          | Con Integer
          | Term :+: Term
          | Lam Name Term
          | App Term Term
  deriving (Show)

pgm :: Term
pgm = App
  (Lam "y"
    (App
      (App
        (Lam "f"
          (Lam "y"
            (App (Var "f") (Var "y"))
          )
        )
        (Lam "x"
          (Var "x" :+: Var "y")
        )
      )
      (Con 3)
    )
  )
  (Con 4)


data Value = Num Integer
           | Fun (Value -> M Value)
           | Wrong

instance Show Value where
 show (Num x) = show x
 show (Fun _) = "<function>"
 show Wrong   = "<wrong>"

type Environment = [(Name, Value)]

interp :: Term -> Environment -> M Value
interp (Var x) env = lookupM x env
interp (Con const) _ = return (Num const)
--interp (Fail) _ = []
--interp (Amb x1 x2) env = interp x1 env ++ interp x2 env
interp (x1 :+: x2) env = do
                          v1 <- interp x1 env
                          v2 <- interp x2 env
                          add v1 v2
interp (Lam x exp) env = return $ Fun $ \v -> interp exp ((x,v):env)
interp (App x1 x2) env = do
                          f <- interp x1 env
                          val <- interp x2 env
                          apply f val



add :: Value -> Value -> M Value
add (Num i) (Num j) = return (Num (i + j))
add _ _ = return Wrong



apply :: Value -> Value -> M Value
apply (Fun k) v = k v
apply _ _ = return Wrong



lookupM x env = case lookup x env of     
              Just val -> return val     
              Nothing ->  return Wrong

-- Functia de testare
test :: Term -> String
test t = showM $ interp t []

pgm1:: Term
pgm1 = App
          (Lam "x" ((Var "x") :+: (Var "x")))
          ((Con 10) :+:  (Con 11))
-- test pgm => "7"
-- test pgm1 => "42"

--program gresit
pgm2 :: Term
-- trece de sintaxa deoarece sintaxa e f permisiva pt cum am definit programelul
pgm2 = (App (Lam "x" (Var "y" :+: Var "x"))(Con 10 :+: Con 11))
-- test pgm2 => "<wrong>"
-- showM $ interp pgm2 [("y", Num 4)] => "25" -> corectare prin mediul de evaluare
-- showM $ interp pgm2 [("z", Num 4)] => "<wrong>"
-- showM $ interp pgm2 [("y", Num 4), ("z", Num 4)] => "25"

--alt program gresit
pgm3 :: Term
pgm3 = App (Con 10) (Con 11)
-- showM $ interp pgm3 [] => "<wrong>" -> indiferent ce mediu de evaluare punem