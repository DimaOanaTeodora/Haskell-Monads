--- Monada Writer List
-- Writer este constructorul 
-- cu runWriter se face afisarea
newtype WriterL a = Writer { runWriter :: (a, [String]) } 

instance  Monad WriterL where
  return va = Writer (va, [])
  ma >>= k = let (va, log1) = runWriter ma
                 (vb, log2) = runWriter (k va)
             in  Writer (vb, log1 ++ log2)

instance  Applicative WriterL where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor WriterL where              
  fmap f ma = pure f <*> ma     

tell :: String -> WriterL () -- functie auxiliara
tell log = Writer ((), [log])
  
logIncrement :: Int  -> WriterL Int
logIncrement x = Writer (x+1, ["Am incrementat " ++ show(x)])
-- runWriter (logIncrement 3) => (4,"Am incrementat 3 ")

-- Varianta DO ---
logIDo :: Int  -> WriterL Int
logIDo x = do
      tell("Am incrementat " ++ show(x))
      return (x+1)

-- varianta DO1
logIncrementN :: Int -> Int -> WriterL Int
logIncrementN x 1 = logIncrement x
logIncrementN x n = do 
                  y <- logIncrementN x (n-1)
                  logIncrement y
-- runWriter $ logIncrementN 2 4

-- varianta DO2
logINDo:: Int -> Int -> WriterL Int
logINDo x n =  do
      tell ("Am incrementat " ++ show x)
      if n > 1 then logINDo (x+1) (n-1)
      else return (x+1)

isPos :: Int -> WriterL Bool
isPos x = if (x>= 0) then (Writer (True, ["poz"])) else (Writer (False, ["neg"]))
-- map isPos [1,2,3] => EROARE deoarece are rezultate monadice care au nevoie de runWriter la afisare
-- map :: (a -> b) -> [a] -> [b]
-- map runWriter $ map isPos [1,-2,3] => [(True,["poz"]),(False,["neg"]),(True,["poz"])]

mymap :: (a -> WriterL b) -> [a] -> WriterL [b] -- ca la Maybe doar ca schimb in definitia functiei
mymap f [] = return []
mymap f (x:xs) = do
                  y <- f x 
                  ys <- mymap f xs
                  return (y:ys)

-- runWriter $ mymap isPos [1,-2,3] => ([True,False,True],["poz","neg","poz"])
