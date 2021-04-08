--- Limbajul si  Interpretorul
-- Monada listelor

type M a = [a]

showM :: M Value -> String
showM [Wrong] =  error "wrong"
showM s = show s

type Name = String

data Term = Var Name
          | Con Integer
          | Term :+: Term
          | Amb Term Term --adaug aici
          | Fail --adaug aici
          | Ned [Term] -- expresie formata din oricate valori vrem
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
interp (Fail) _ = [] -- adaug aici
interp (Amb x1 x2) env = (interp x1 env) ++ (interp x2 env) --adaug aici
interp (Ned xs) env = concat [interp t env| t<- xs] --adaug aici
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
          (Amb (Con 10)(Con 11)) --modific aici
-- test pgm => "[20,22]"
-- test pgm1 => "[42]"

--program gresit
pgm2 :: Term
-- trece de sintaxa deoarece sintaxa e f permisiva pt cum am definit programelul
pgm2 = (App (Lam "x" (Var "y" :+: Var "x"))(Con 10 :+: Con 11))
-- test pgm2 => "[<wrong>]"
-- showM $ interp pgm2 [("y", Num 4)] => "[25]" -> corectare prin mediul de evaluare
-- showM $ interp pgm2 [("z", Num 4)] => "[<wrong>]"
-- showM $ interp pgm2 [("y", Num 4), ("z", Num 4)] => "[25]"

--alt program gresit
pgm3 :: Term
pgm3 = App (Con 10) (Con 11)
-- showM $ interp pgm3 [] => "<wrong>" -> indiferent ce mediu de evaluare punem

-- return 3 :: [Int] => [3]
-- [3,4] >>= (\x -> [x+x, x*x, x+1]) => [6,9,4,8,16,5]

pgm4 :: Term
pgm4 = ( Amb (Con 1) (Con 2)) :+: Amb (Con 3) (Con 4)
-- tets pgm4 => "[4,5,5,6]"