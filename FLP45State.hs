newtype InState  a = State {runState :: Integer -> (a, Integer)}

--- Monada State

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
get = State ( \s -> (s,s)) -- functie asemanatoare cu ask

modify :: (Integer -> Integer) -> InState () --doar schimba starea altceva nu face
modify f = State (\s -> ((), f s))

--- Limbajul si  Interpretorul

type M = InState

-- modul in care scot rezultatul din valoarea monadic : (va, news) = (runState ma s )
showM :: Show a => M a -> String
showM ma = let (va, s)=(runState ma 0 ) in ("Value: " ++ (show va) ++" Count: " ++ (show s))
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
          | Count -- modific aici
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
interp Count _ = do --adaug aici - count nu depinde de mediu (env) pt ca el e starea 
                  s <- get
                  return (Num s) --transform in Value
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
add (Num i) (Num j) = modify(+1) >>  return (Num (i + j)) --trebuie sa maresc count-ul ca altfel merge dar e mereu 0
add _ _ = return Wrong



apply :: Value -> Value -> M Value
apply (Fun k) v = modify(+1) >>  k v --si aici modific starea (count-ul) prin modify
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
-- test pgm => "Value: 7 Count: 5" --face operatia cu valoarea anterioara
-- test pgm1 => "Value: 42 Count: 3"

--program gresit
pgm2 :: Term
-- trece de sintaxa deoarece sintaxa e f permisiva pt cum am definit programelul
pgm2 = (App (Lam "x" (Var "y" :+: Var "x"))(Con 10 :+: Con 11))
-- test pgm2 => "Value: <wrong> Count: 2"
-- showM $ interp pgm2 [("y", Num 4)] => "Value: 25 Count: 3" -> corectare prin mediul de evaluare
-- showM $ interp pgm2 [("z", Num 4)] => "Value: <wrong> Count: 2"
-- showM $ interp pgm2 [("y", Num 4), ("z", Num 4)] => "Value: 25 Count: 3"

--alt program gresit
pgm3 :: Term
pgm3 = App (Con 10) (Con 11)
-- showM $ interp pgm3 [] => "<wrong>" -> indiferent ce mediu de evaluare punem