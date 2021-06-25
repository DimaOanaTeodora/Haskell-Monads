type Val = Int
data Operatie = Add
      | Head 
data Lit = V Val | Id String
data Arb
   = Leaf Lit
   | Node Operatie [Arb]

type Stare = [(String, Val)]   -- valori pentru identificatorii (`Id x`) din arbore

val :: Lit -> Stare -> Val
val (V v) _ = v
val (Id s) env = aux (lookup s env)
      where
            aux (Just v) = v 
            aux Nothing = error "variabila negasita"

eval ::  Arb -> Stare -> Val
eval (Leaf v) env = val v env
eval (Node Add []) env = 0
eval (Node Add (h:t)) env = (eval h env) + (eval (Node Add t) env)
eval (Node Head []) env = error "lista vida" 
eval (Node Head (h:t)) env = eval h env


test :: Val -> Bool
test = (/= 0)


test1 = (Node Add [(Leaf (V 1)) ,(Leaf (V 1)),(Leaf (V 1)),(Leaf (V 1))])
-- eval test1 [] = 4
test2 = (Node Add [(Leaf (Id "a")) ,(Leaf (Id "a"))])
{- eval test2 [("x",0)] = 
*Main> eval test2 [("x",0)]
*** Exception: variabila negasita
CallStack (from HasCallStack):-}
test3 = (Node Head [(Leaf (V 1))])
-- eval test3 [] = 1
test4 = (Node Head [])
{-
*Main> eval test4 []
*** Exception: lista vida
CallStack (from HasCallStack):
-}

type M = Maybe

mval :: Lit -> Stare -> M Val
mval (V v) _ = return v
mval (Id s) env = lookup s env

meval ::  Arb -> Stare -> M Val
meval (Leaf v) env = mval v env
meval (Node Add []) env = Just 0
meval (Node Add (h:t)) env = do
      n1 <- meval h env
      n2 <- meval (Node Add t) env
      return (n1 + n2)
meval (Node Head []) env = Nothing
meval (Node Head (h:t)) env = meval h env

mtest :: M Val -> Bool
mtest (Just x) = x /=0

mtest1 = (Node Add [(Leaf (V 1)) ,(Leaf (V 1)),(Leaf (V 1)),(Leaf (V 1))])
-- meval mtest1 [] = Just 4
mtest2 = (Node Add [(Leaf (Id "a")) ,(Leaf (Id "a"))])
-- meval mtest2 [("x",0)] = Nothing
mtest3 = (Node Head [(Leaf (V 1))])
-- meval mtest3 [] = Just 1
mtest4 = (Node Head [])
-- meval mtest4 [] = Nothing
