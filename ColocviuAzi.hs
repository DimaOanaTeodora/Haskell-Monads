type Val = Int
data Operatie = Add
      | If Operatie Operatie 
data Lit = V Val | Id String
data Arb
   = Leaf Lit
   | Node Operatie [Arb]

type Stare = [(String, Val)]   -- valori pentru identificatorii (`Id x`) din arbore

val :: Lit -> Stare -> Val
-- Lit -> [(String, Int)] -> Int
val (V v) _ = v
val (Id s) env = aux (lookup s env)
      where
            aux (Just v) = v 
            aux Nothing = error "variabila negasita"
-- val (Id "n") [("n", 2)] => 2

eval ::  Arb -> Stare -> Val
-- Arb -> [(String, Int)] -> Int
eval (Leaf v) env = val v env
eval (Node Add []) env = 0
eval (Node Add (x:xs)) env = (eval x env) + (eval (Node Add xs) env)
eval (Node (If op1 op2) [] ) env = error "lista vida"
eval (Node (If op1 op2) (x:xs)) env = if test (eval x env) then eval (Node op1 xs) env else eval (Node op2 xs) env


test :: Val -> Bool
test = (/= 0)

arb1 = (Node Add [(Leaf (V 2)) ,(Leaf (V 3))])
-- eval arb1 [] => 5
arb2 = (Node Add [(Leaf (V 2)) ,(Leaf (V 3)),(Leaf (V 6))])
-- eval arb2 [] => 11
arb3 = (Node (If Add Add) [(Leaf (V 2)) ,(Leaf (V 3)),(Leaf (V 6))])
-- eval arb3 [] => 9
arb4 = (Node Add [(Leaf (Id "n")) ,(Leaf (Id "p"))])
-- eval arb4 [("n",2), ("p", 1)] => 3
arb5 = (Node Add [(Leaf (Id "n")) ,(Leaf (Id "p"))])
-- eval arb5 [("x",2), ("p", 1)] => "Variabila negasita"

type M = Maybe

maybeVal :: Lit -> Stare -> M Val
maybeVal (V v) _ = return v
maybeVal (Id s) env = lookup s env
-- maybeVal (Id "n") [("n", 2)] => Just 2

maybeEval ::  Arb -> Stare -> M Val
-- Arb -> [(String, Int)] -> Int
maybeEval (Leaf v) env = maybeVal v env
maybeEval (Node Add []) env = Just 0
maybeEval (Node Add (x:xs)) env = do
      x <- maybeEval x env
      y <- maybeEval (Node Add xs) env
      return (x+y)
maybeEval (Node (If op1 op2) [] ) env = Nothing
maybeEval (Node (If op1 op2) (x:xs)) env = if maybeTest  (maybeEval x env) then maybeEval (Node op1 xs) env else maybeEval (Node op2 xs) env


maybeTest :: M Val -> Bool
maybeTest (Just x) = x /=0

arb6 = (Node Add [(Leaf (V 2)) ,(Leaf (V 3))])
-- maybeEval arb6 [] => Just 5
arb7 = (Node Add [(Leaf (V 2)) ,(Leaf (V 3)),(Leaf (V 6))])
-- maybeEval arb7 [] => Just 11
arb8 = (Node (If Add Add) [(Leaf (V 2)) ,(Leaf (V 3)),(Leaf (V 6))])
-- maybeEval arb8 []=> Just 9
arb9 = (Node Add [(Leaf (Id "n")) ,(Leaf (Id "p"))])
-- maybeEval arb9 [("n",2), ("p", 1)] => Just 3
arb10 = (Node Add [(Leaf (Id "n")) ,(Leaf (Id "p"))])
-- maybeEval arb10 [("x",2), ("p", 1)] => Nothing
arb11 = (Node (If Add Add) [])
-- maybeEval arb11 [] => Nothing
