type Env = (Int,Int)   -- corespunzator celor doua celule de memorie

data Prog  = On Env Stmt  -- Env reprezinta valorile initiale ale celulelor de memorie    

data Stmt = Off
    | Expr :<< Stmt -- evalueaza Expr, pune rezultatul in Mem1, apoi executa Stmt
    | Expr :< Stmt  -- evalueaza Expr, pune rezultatul in Mem2, apoi executa Stmt

data Expr  =  M Mem 
  | V Int 
  | Expr :+ Expr 
  | If1 Expr Expr 
  | If2 Expr Expr

data Mem = Mem1 | Mem2 

infixl 6 :+
infixr 2 :<
infixr 2 :<<

expr ::  Expr -> Env -> Int -- Expr -> (Int, Int) -> Int
expr (M Mem1) env = let (m1, m2) = env in m1
expr (M Mem2) env = let (m1, m2) = env in m2
expr (V n) env = n
expr (exp1 :+ exp2) env = (expr exp1 env) + (expr exp2 env)
expr (If1 exp1 exp2) env = let (m1, m2) = env in if m1 /= 0 then (expr exp1 env) else(expr exp2 env)
expr (If2 exp1 exp2) env = let (m1, m2) = env in if m2 /= 0 then (expr exp1 env) else(expr exp2 env)


stmt :: Stmt -> Env -> Env -- Stmt -> (Int, Int) -> (Int, Int)
stmt Off env = env
stmt (e :<< stm) m= let eval = expr e m
                        (m1, m2) = m 
                    in stmt stm (eval, m2) 
stmt (e :< stm) m= let eval = expr e m
                       (m1, m2) = m 
                    in stmt stm (m1, eval) 


prog :: Prog -> Env
prog (On env st)= stmt st env
-- testele
