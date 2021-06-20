--- Monada Writer
-- Writer este constructorul 
-- cu runWriter se face afisarea
newtype WriterS a = Writer { runWriter :: (a, String) } 


instance  Monad WriterS where
  return va = Writer (va, "")
  ma >>= k = let (va, log1) = runWriter ma
                 (vb, log2) = runWriter (k va)
             in  Writer (vb, log1 ++ log2)


instance  Applicative WriterS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor WriterS where              
  fmap f ma = pure f <*> ma     

tell :: String -> WriterS () -- functie auxiliara
tell log = Writer ((), log)
  
logIncrement :: Int  -> WriterS Int
logIncrement x = Writer (x+1, "Am incrementat " ++ show(x) ++ " ")
-- runWriter (logIncrement 3) => (4,"Am incrementat 3 ")

-- Varianta DO ---
logIDo :: Int  -> WriterS Int
logIDo x = do
      tell("Am incrementat " ++ show(x) ++ " ")
      return (x+1)

-- varianta DO1
logIncrementN :: Int -> Int -> WriterS Int
logIncrementN x 1 = logIncrement x
logIncrementN x n = do 
                  y <- logIncrementN x (n-1)
                  logIncrement y
-- runWriter $ logIncrementN 2 4

-- varianta DO2
logINDo:: Int -> Int -> WriterS Int
logINDo x n =  do
      tell ("Am incrementat " ++ show x ++",")
      if n > 1 then logINDo (x+1) (n-1)
      else return (x+1)
