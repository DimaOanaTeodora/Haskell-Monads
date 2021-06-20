import Data.Maybe
import Data.List 

type Name = String

data Pgm = Pgm [Name] Stmt
      deriving (Read, Show)

data Stmt = Skip 
      | Stmt ::: Stmt 
      | If BExp Stmt Stmt 
      | While BExp Stmt 
      | Name := AExp
      deriving (Read, Show)

data BExp = BTrue 
      | BFalse 
      | AExp :==: AExp 
      | Not BExp
      deriving (Read, Show)

data AExp = Lit Integer 
      | AExp :+: AExp 
      | AExp :*: AExp 
      | Var Name
      deriving (Read, Show)

infixr 2 :::
infix 3 :=
infix 4 :==:
infixl 6 :+:
infixl 7 :*:

type Env = [(Name, Integer)] -- [(String, Integer)]
factStmt :: Stmt
factStmt = "p" := Lit 1 ::: "n" := Lit 3::: While (Not (Var "n" :==: Lit 0))( "p" := Var "p" :*: Var "n" :::"n" := Var "n" :+: Lit (-1))
pg1 = Pgm [] factStmt -- => [("n",0),("p",6)]

pEval :: Pgm -> Env
pEval (Pgm lista s) = sEval s [(x,0) | x<- lista] -- zice ca sunt initializate cu 0 la inceput 

sEval :: Stmt -> Env -> Env -- Stmt ->[(String, Integer)] -> [(String, Integer)]
sEval Skip env = env
sEval (s1 ::: s2) env = sEval s2 (sEval s1 env) 
sEval (If bexp s1 s2) env = aux (bEval bexp env ) s1 s2
      where
            aux boolean s1 s2 = if boolean then sEval s1 env else sEval s2 env
sEval (While bexp s) env =  if bEval bexp env then sEval (s ::: While bexp s) env else sEval Skip env
sEval (string := aexp) env = (string, aEval aexp env) : [(x, value) | (x, value) <- env, x /= string ]

bEval :: BExp -> Env -> Bool
bEval BTrue _ = True
bEval BFalse _ = False
bEval (a1 :==: a2) env = (aEval a1 env) == (aEval a2 env)
bEval (Not bexp) env = not (bEval bexp env)

aEval :: AExp -> Env -> Integer -- AExp ->[(String, Integer)] -> Integer
aEval (Lit n) _ = n 
aEval (a1 :+: a2) env = (aEval a1 env) + (aEval a2 env)
aEval (a1 :*: a2) env = (aEval a1 env) * (aEval a2 env)
aEval (Var string ) env = aux (lookup string env)
      where
            aux (Just value) = value
            aux Nothing = error "Erroare lookup"
env :: Env
env = []

aexp :: AExp 
aexp = (Lit 5) :+: (Lit 2)

bexp :: BExp 
bexp = Not (aexp :==: aexp )
