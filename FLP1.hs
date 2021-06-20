--- LABORATOR 1 ---
-- Mediu de evaluare (Env) == memoria
data Prog = On Instr

data Instr = Off | Expr :> Instr

data Expr = Mem | V Int | Expr :+ Expr

type Env = Int -- valoarea celulei de memorie

type DomProg = [Int]

type DomInstr = Env -> [Int] -- Int -> [Int]

type DomExpr = Env -> Int -- Int -> Int

p1 :: Prog
p1 = On ((V 3) :> ((Mem :+ (V 5)) :> Off))

prog :: Prog -> DomProg -- Prog -> [Int] 
prog (On instr) = stmt instr 0

stmt :: Instr -> DomInstr -- Instr -> Int -> [Int]
stmt Off n = []
stmt (e :> instr) n = exp : (stmt instr exp)
      where exp = expr e n

expr :: Expr -> DomExpr -- Expr -> Int -> Int
expr Mem n = n
expr (V x) n = x
expr (e1 :+ e2) n = (expr e1 n) + (expr e2 n) 


--------------Mini Haskell------------
type Name = String

data Hask = HTrue
      | HFalse
      | HLit Int
      | HIf Hask Hask Hask
      | Hask :==: Hask
      | Hask :+: Hask
      | HVar Name -- HVar String
      | HLam Name Hask --HLam String Hask
      | Hask :$: Hask
      deriving (Read, Show)

infix 4 :==:
infixl 6 :+:
infixl 9 :$:

data Value = VBool Bool
      | VInt Int
      | VFun (Value -> Value)
      | VError -- pentru reprezentarea erorilor
type HEnv = [(Name, Value)] --[(String, Value)]
type DomHask = HEnv -> Value --[(String, Value)] -> Value

instance Show Value where
      show (VBool bool) = show bool
      show (VInt n) = show n
      show (VFun fun) = "functie"
      show VError = "eroare"  

instance Eq Value where
      (VInt n1) == (VInt n2) = n1 == n2 
      (VBool b1) == (VBool b2) = b1 == b2 
      _ == _ = error "eroare la egalitate"

hEval :: Hask -> DomHask -- Hask -> [(String, Value)] -> Value
hEval HTrue _ = VBool True
hEval HFalse _ = VBool False
hEval (HLit n) _ = VInt n

hEval (HIf h1 h2 h3) env = auxif (hEval h1 env) (hEval h2 env) (hEval h3 env)
      where
            auxif (VBool b) h1 h2 = if b then h1 else h2
            auxif _ _ _ = VError
hEval (h1 :==: h2) env = auxeq (hEval h1 env) (hEval h2 env)
      where
            auxeq (VBool b1) (VBool b2) = VBool (b1 == b2)
            auxeq (VInt b1) (VInt b2) = VBool (b1 == b2)
            auxeq _ _ = VError
hEval (h1 :+: h2) env = auxsum (hEval h1 env) (hEval h2 env)
      where 
            auxsum (VInt n1) (VInt n2) = VInt (n1+n2)
            auxsum _ _ = VError
-- 	lookup :: a -> [(a,b)] -> Maybe b
hEval (HVar string) env = auxf (lookup string env)
      where 
            auxf (Just x) = x
            auxf Nothing = VError
            
hEval (HLam string h) env = VFun (\v -> hEval h ((string,v): env))
hEval (h1 :$: h2) env = auxapp (hEval h1 env) (hEval h2 env)
      where
            auxapp (VFun f) val = f val
            auxapp _ _ = VError

h0 :: Hask
h0= (HIf (HTrue) (HLit 1) (HLit 2)) 
henv = []
