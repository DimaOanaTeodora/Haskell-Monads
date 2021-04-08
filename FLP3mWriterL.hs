
--- Monada Writer

    

newtype WriterLS a = Writer { runWriter :: (a, [String]) } 


instance  Monad WriterLS where
  return va = Writer (va, [])
  ma >>= k = let (va, log1) = runWriter ma
                 (vb, log2) = runWriter (k va)
             in  Writer (vb, log1 ++ log2)


instance  Applicative WriterLS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor WriterLS where              
  fmap f ma = pure f <*> ma 



tell :: String -> WriterLS () 
tell log = Writer ((), [log])
  
logIncrement :: Int  -> WriterLS Int
logIncrement x = Writer(x+1, ["Am incrementat " ++ (show x) ++ " "])
-- runWriter $ (logIncrementN 3 4) => (7,["Incrementam: 3","Incrementam: 4","Incrementam: 5","Incrementam: 6"])


-- nu se schimba fata de varianta initiala 
logIncrementN :: Int -> Int -> WriterLS Int
logIncrementN x n =  do
                      tell ("Incrementam: " ++ show x)
                      if n > 1 then logIncrementN (x+1) (n-1)
                      else return (x+1)
                         
isPos :: Int -> WriterLS Bool
isPos x = if (x>= 0) then (Writer (True, ["poz"])) else (Writer (False, ["neg"]))
-- nu pot sa-i afisez rezultatul pentru ca nu are show                           
-- map isPos [1,2,3] => EROARE
-- map runWriter $ map isPos [1,2,3] => [(True,["poz"]),(True,["poz"]),(True,["poz"])]
--                                    => lista de valori monadice
mapWriterLS :: (a -> WriterLS b) -> [a] -> WriterLS [b]
mapWriterLS f [] = return []
mapWriterLS f (x:xs) = do
                        y <- f x -- f x e valoarea monadica deci trebuie sa extrag din el folsind '<-'
                        ys <- mapWriterLS f xs
                        return (y:ys)
-- runWriter $ mapWriterLS isPos [1,-2,3] => ([True,False,True],["poz","neg","poz"])

-- Vraianta fara definitie monadica
--mapWriterLS f xs = let ys = map f xs in Writer ( [fst (runWriter y) | y <- ys], concat [snd (runWriter y)| y<- ys])