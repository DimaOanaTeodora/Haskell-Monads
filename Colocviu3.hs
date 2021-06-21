{-
Finalizati definitia functiilor de interpretare.
Adaugati instructiunea While BExp Stmt si interpretarea ei.
-}
import Data.Maybe
import Data.List

type Name = String

data Pgm  = Pgm [Name] Stmt
        deriving (Read, Show)

data BExp = BTrue 
  | BFalse 
  | AExp :==: AExp 
  | Not BExp
  deriving (Read, Show)

data Stmt = Skip 
  | Stmt ::: Stmt 
  | If BExp Stmt Stmt
  | Name := AExp
  | While BExp Stmt
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

aEval :: AExp -> Env -> Integer
aEval (Lit n) _ = n
aEval (aexp1 :+: aexp2) env = (aEval aexp1 env) + (aEval aexp2 env)
aEval (aexp1 :*: aexp2) env = (aEval aexp1 env) * (aEval aexp2 env)
aEval (Var string) env = aux (lookup string env)
  where
    aux (Just n) = n 
    aux Nothing = error "Eroare lookup"

bEval :: BExp -> Env -> Bool
bEval BTrue env = True
bEval BFalse env = False
bEval (aexp1 :==: aexp2) env = (aEval aexp1 env) == (aEval aexp2 env)
bEval (Not bexp) env = not (bEval bexp env)

sEval :: Stmt -> Env -> Env
sEval Skip env = env
sEval (st1 ::: st2) env = (sEval st1 env) ++ (sEval st2 env)
sEval (If b st1 st2) env =  if (bEval b env) then (sEval st1 env) else (sEval st2 env) 
sEval (string := aexp) env = (string, aEval aexp env) : [(x, value) | (x, value) <- env, x /= string ]
sEval (While bexp st) env = if bEval bexp env then sEval (st ::: While bexp st) env else sEval Skip env


pEval :: Pgm -> Env
pEval (Pgm lvar st) = sEval st [(x,0) | x<- lvar] --  initializate memorie cu 0 


 
factStmt :: Stmt
factStmt =
  "p" := Lit 1 ::: "n" := Lit 3 :::
  While (Not (Var "n" :==: Lit 0))
    ( "p" := Var "p" :*: Var "n" :::
      "n" := Var "n" :+: Lit (-1)
    )
test2 = Pgm ["p", "n"] ("p" := Var "p" :*: Var "n" :::"n" := Var "n" :+: Lit (-1))
-- [("p",0),("n",0),("n",-1),("p",0)]

test1 = Pgm ["p", "n"] factStmt 
-- [("p",1),("n",0),("n",3),("p",0),("p",0),("n",0)] 

{-CERINTE

1) (10pct) Finalizati definitia functiilor de interpretare.
2) (10pct) Adaugati instructiunea While BExp Stmt si interpretarea ei.
3) (20pct) Definiti interpretarea limbajului astfel incat programele sa se execute dintr-o stare 
initiala data iar  pEval  sa afiseze starea initiala si starea finala.

Definiti teste pentru verificarea solutiilor si indicati raspunsurile primite. 

-}
