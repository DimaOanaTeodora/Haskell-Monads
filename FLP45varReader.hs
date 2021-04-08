
--- Monada Reader
newtype EnvReader  a = Reader { runReader :: Environment -> a}

-------------------

instance Monad EnvReader where
  return va = Reader (\ _ -> va) 
  ma >>= k =  Reader g -- k :: a -> EnvReader a 
              where
                g env = let va = runReader ma env in runReader (k va) env
  
  -- ma :: Reader f unde f :: Environment -> a f = runReader ma

instance Applicative EnvReader where
  pure = return
  mf <*> ma = do
              f <-mf
              a <-ma
              return (f a)

instance Functor EnvReader where
  fmap f ma = pure f <*> ma

-- vreau sa intorc mediul ca valoarea
ask :: EnvReader Environment
ask = Reader id

-- ne da voie sa schimbam mediul de evaluare
local :: (Environment -> Environment) -> EnvReader a -> EnvReader a
local f ma =  Reader g
              where
                g env = (runReader ma) (f env)  

--- Limbajul si  Interpretorul

type M = EnvReader 

showM :: Show a => M a -> String
showM ma = show $ runReader ma []

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

interp :: Term -> M  Value-- interp t :: Reader Environment Value
                          -- interp t = Reader f
interp (Var x) = lookupM x 
interp (Con const)  = return (Num const)
interp (x1 :+: x2)  = do
                          v1 <- interp x1 
                          v2 <- interp x2 
                          add v1 v2
-- atentie aici ce sterg nu merge sters doar env
interp (Lam x exp) = do
                      env <- ask -- pun mana pe env cu ask
                      return $ Fun  (\v -> let f = \env -> ((x,v): env) in (local f ( interp exp)))
                      -- Fun :: Value -> M Value
interp (App x1 x2)  = do
                          f <- interp x1 
                          val <- interp x2 
                          apply f val



add :: Value -> Value -> M Value
add (Num i) (Num j) = return (Num (i + j))
add _ _ = return Wrong



apply :: Value -> Value -> M Value
apply (Fun k) v = k v
apply _ _ = return Wrong


-- Varianta monadica (apare la examen)
lookupM :: Name ->  M Value -- asta inseamna: lookupM :: Name ->  EnvReader M Value
lookupM x  = do
                env <- ask
                case lookup x env of     
                    Just val -> return val     
                    Nothing -> return Wrong

-- Varianta nemonadica
-- lookupM :: Name ->  M Value -- asta inseamna: lookupM :: Name ->  EnvReader M Value
-- lookupM x  = Reader f
--               where
--                 f env = case lookup x env of     
--                         Just val ->  val     
--                         Nothing -> Wrong
-- Verificare lookup
-- runReader (lookupM "x") [("y", Num 4)] => <wrong> pt ca nu-l contine pe x
-- runReader (lookupM "x") [("y", Num 4), ("x", Num 3)] => 3

-- Functia de testare
test :: Term -> String
test t = showM $ interp t  --modific si aici

pgm1:: Term
pgm1 = App
          (Lam "x" ((Var "x") :+: (Var "x")))
          ((Con 10) :+:  (Con 11))
-- test pgm => "<wrong>"
-- test pgm1 => "42"

--program gresit
pgm2 :: Term
-- trece de sintaxa deoarece sintaxa e f permisiva pt cum am definit programelul
pgm2 = (App (Lam "x" (Var "x" :+: Var "x"))(Con 10 :+: Con 11))
-- test pgm2 => "42"


--alt program gresit
pgm3 :: Term
pgm3 = App (Con 10) (Con 11)
-- test pgm3 => "<wrong>"

--show $ runReader (interp pgm2) [("y", Num 4)] => "42"