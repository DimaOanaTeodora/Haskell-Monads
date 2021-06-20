
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
           | Fun (Value -> M Value) -- M de tipul Identity
           | Wrong
      
tell:: String -> StringWriter () 
tell mesaj = StringWriter ((), mesaj)
--------------------------------------------------
-- Identity instanta a clasei Show
instance Show Value where -- ma uit la data Value
 show (Num x) = show x
 show (Fun _) = "<function>"
 show Wrong   = "<wrong>"

type Environment = [(Name, Value)]
-- se poate inlocui Environment cu [(Name, Value)]
-- interp :: Term -> Environment -> M Value
-- unde Name se poate inlocui si el cu String
-- definire interpretor ca in primele laburi 
interp :: Term -> [(Name, Value)] -> M Value
interp (Var name) env = lookupM name env -- fac propriul lookup
interp (Con const) _ = return (Num const) -- intoarce un M(return ) Value(Num) 
interp (Out x) env = return (interp x env) ++ ";"
interp (x1 :+: x2) env = do
                          -- le scoate din cutie de aceea v1 si v2 sunt de tip Value
                          v1 <- interp x1 env
                          v2 <- interp x2 env
                          return v1 ++ v2
interp (Lam name exp) env = return $ Fun $ \v -> interp exp ((name,v):env)
interp (App x1 x2) env = do
                          f <- interp x1 env
                          val <- interp x2 env
                          apply f val
-- Functii auxiliare: 

apply :: Value -> Value -> M Value
apply (Fun k) v = k v
apply _ _ = return Wrong

-- ma folosesc de lookup normal 
lookupM name env = case lookup name env of     
              Just val -> return val     
              Nothing ->  return Wrong

----------------------- Functia de testare------------------------
showM :: Show a => M a -> String
showM ma = show (runStringWriter ma) -- in notatie functionala compunere: show .runIdentity
-- interp pgm [] => EROARE
-- showM $ interp pgm [] => "7"
-- showM $ interp pgm1 [] => "42"

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