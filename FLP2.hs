---- LABORATOR 2-----
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

type Env = [(Name, Integer)]

factStmt :: Stmt
factStmt = "p" := Lit 1 ::: "n" := Lit 3:::
  While (Not (Var "n" :==: Lit 0))
  ( "p" := Var "p" :*: Var "n" ::: "n" := Var "n" :+: Lit (-1))

pg1 = Pgm [] factStmt -- [("n",0),("p",6)]

pEval :: Pgm -> Env
pEval (Pgm lista st) = sEval st [(x,0) | x<- lista]

sEval :: Stmt -> Env -> Env -- Stmt -> [(String, Integer)] -> [(String, Integer)]
sEval Skip env = env
sEval (st1 ::: st2) env = sEval st2 (sEval st1 env) 
sEval (If bexp st1 st2) env = if (bEval bexp env) then sEval st1 env else sEval st2 env
sEval (string := aexp) env = (string, aEval aexp env) : [(x, value) | (x, value) <- env, x /= string ]
sEval (While bexp st) env =  if bEval bexp env then sEval (st ::: While bexp st) env else sEval Skip env

bEval :: BExp -> Env -> Bool -- BExp -> [(String, Integer)] -> Bool
bEval BTrue env = True
bEval BFalse env = False
bEval (aexp1 :==: aexp2) env = (aEval aexp1 env) == (aEval aexp2 env)
bEval (Not bexp) env = not (bEval bexp env)
-- bEval (((Var "n") :+: (Var "p")) :==: ((Var "n") :+: (Var "p"))) [("n", 2),("p", 3)] => True

aEval :: AExp -> Env -> Integer -- AExp -> [(String, Integer)] -> Integer
aEval (Lit n) env = n 
aEval (aexp1 :+: aexp2) env = (aEval aexp1 env) + (aEval aexp2 env)
aEval (aexp1 :*: aexp2) env = (aEval aexp1 env) * (aEval aexp2 env)
aEval (Var string) env = aux (lookup string env)
  where
    aux (Just n) = n 
    aux Nothing = error "Eroare lookup"
-- aEval ((Var "n") :+: (Var "p")) [("n", 2),("p", 3)] => 5

