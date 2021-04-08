-- LABORATOR 1
--limbajul unui mini calculator (limbaj imperativ)
--Mem si Off constructori vizi
data Prog = On Instr --constructor de tip, marcheaza inceputul programului si ajuta la definitie
data Instr = Off | Expr :> Instr --vreau un sir de expresii, echivalenta cu MyList=Nil | Elem :> List (elem urmat de list) 
data Expr = Mem | V Int | Expr :+ Expr
type Env = Int -- valoarea celulei de memorie
type DomProg = [Int]
type DomInstr = Env -> [Int]
type DomExpr = Env -> Int

prog :: Prog -> DomProg
prog (On sir_instructiuni) = stmt sir_instructiuni 0 --apelez instructiunea din starea memoriei 0

--domeniul Instr este o functie
stmt :: Instr -> DomInstr
stmt Off _ = []
--stmt s =\m -> definitie echivalent cu ce e mai jos
stmt (expresie :> sir_instructiuni) celula_memorie = exp: stmt sir_instructiuni exp
  where
    exp=expr expresie celula_memorie

expr :: Expr -> DomExpr
expr Mem celula_memorie = celula_memorie
expr (V n) celula_memorie = n
expr (e1 :+ e2) celula_memorie = (expr e1 celula_memorie) + (expr e2 celula_memorie) 

--exemplu de programel
-- :t p1 o sa afiseze p1::Prog
p1 = On ( (V 3) :> ((Mem :+ (V 5)):> Off))

--mini Haskell (limbaj functional)
type Name = String
data Hask = HTrue
 | HFalse
 | HLit Int
 | HIf Hask Hask Hask
 | Hask :==: Hask
 | Hask :+: Hask
 | HVar Name
 | HLam Name Hask
 | Hask :$: Hask --operator de aplicare functie $ pe_cine 
  deriving (Read, Show)
infix 4 :==:
infixl 6 :+:
infixl 9 :$:

--(HLam "x" (HVar "x" :+: HLit 1)) :$: (HLit 2) -> vreau sa l inlocuiesc pe x cu 2 in expresia care urmeaza
data Value = VBool Bool
 | VInt Int
 | VFun (Value -> Value)
 | VError -- pentru reprezentarea erorilor pt toate constructiile care sunt permise prin sintaxa dar sunt eronate
type HEnv = [(Name, Value)]

type DomHask = HEnv -> Value

--a)
instance Show Value where
  show (VBool b)= show b 
  show (VInt val)= show val
  show (VFun f)= "O functie"
  show VError= "Error"

--b)
instance Eq Value where
  (VBool a)==(VBool b)= a==b 
  (VInt a) == (VInt b)= a==b
  _ == _ =error "Error"

--c)
--trebuie sa iau pe cazuru
hEval :: Hask -> DomHask  --cum o gandesc: hEval:: Hask -> HEnv -> Value
hEval HTrue env = VBool True
hEval HFalse env = VBool False
hEval (HLit n) env = VInt n
hEval (HIf expr_booleana e1 e2) env = auxif (hEval expr_booleana env) (hEval e1 env) (hEval e2 env)
  where 
    auxif (VBool b) v1 v2 = if b then v1 else v2
    auxif _ _ _= VError
hEval (e1:==:e2) env=auxi (hEval e1 env)(hEval e2 env )
                      where 
                        auxi (VBool b1)(VBool b2)= VBool (b1==b2)
                        auxi (VInt n1) (VInt n2)=VBool (n1==n2)
                        auxi _ _ = VError
hEval (e1 :+: e2) env= auxsum (hEval e1 env)(hEval e2 env )
                      where 
                        auxsum (VInt n1) (VInt n2)=VInt (n1+n2)
                        auxsum _ _ = VError
hEval (HVar x) env = f (lookup x env)
                      where
                        f (Just a)=a
                        f Nothing =VError
hEval (HLam x e) env=VFun (\v-> hEval e ((x,v):env))
hEval (ef :$: ex) env= auxapp (hEval ef env) (hEval ex env)
                      where
                        auxapp (VFun f) val =f val
                        auxapp _ _ =VError



--hEval ((HLam x e) :$: ex)env
--varinata 2
--hEval expr_booleana env == VBool True 
--  then (hEval e1 env) else if hEval expr_booleana env ==VBool False 
--    then (hEval e2 env) else VError 

h0= (HIf (HTrue) (HLit 1) (HLit 2)) 
-- hEval h0 [] , unde [] este mediul de evaluare
h1 = (HLam "x" (HVar "x" :+: HLit 1)) :$: (HLit 3) -- (\x -> x+1) 3
h2= (HLit 1) :$: (HLit 3) --eroare la interpretare
--hEval (HTrue :==: HFalse)[]
--hEval (HLit 3 :==: HLit 5)[]