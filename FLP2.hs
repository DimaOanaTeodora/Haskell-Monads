-- LABORATOR 2
import Data.Maybe
import Data.List

type Name = String

data  Pgm  = Pgm [Name] Stmt
        deriving (Read, Show)

data Stmt = Skip | Stmt ::: Stmt | If BExp Stmt Stmt | While BExp Stmt | Name := AExp
        deriving (Read, Show)

data AExp = Lit Integer | AExp :+: AExp | AExp :*: AExp | Var Name
        deriving (Read, Show)

data BExp = BTrue | BFalse | AExp :==: AExp | Not BExp
        deriving (Read, Show)

infixr 2 :::
infix 3 :=
infix 4 :==:
infixl 6 :+:
infixl 7 :*:


type Env = [(Name, Integer)]


factStmt :: Stmt
factStmt =
  "p" := Lit 1 ::: "n" := Lit 3 :::
  While (Not (Var "n" :==: Lit 0))
    ( "p" := Var "p" :*: Var "n" :::
      "n" := Var "n" :+: Lit (-1)
    )
    
pg1 = Pgm [] factStmt 


aEval :: AExp -> Env -> Integer
aEval = undefined

bEval :: BExp -> Env -> Bool
bEval = undefined

sEval :: Stmt -> Env -> Env
sEval Skip env = env 
sEval (st1 ::: st2)  env= sEval st2 (sEval st1 env)
sEval (If b st1 st2) env=  if bEval b env then sEval st1 env else sEval st2 env
sEval (While b st) env=  sEval (If b (st ::: While b st) Skip ) env
                        --var 2 if bEval b env then sEval (st ::: While b st) emv else sEval Skip env
--pEval (Pgm ["a"] (Skip :::))

pEval :: Pgm -> Env
pEval (Pgm lista_var lista_instr) = sEval lista_instr [(x,0) | x<- lista_var]


   
