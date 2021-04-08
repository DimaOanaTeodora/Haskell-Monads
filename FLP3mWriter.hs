
--- Monada Writer Writer log a (log - efectul ca unul de logare)

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

tell :: String -> WriterS () 
tell log = Writer ((), log)
  
logIncrement :: Int  -> WriterS Int
logIncrement x = Writer(x+1, "Am incrementat " ++ (show x) ++ " ")
-- runWriter (logIncrement 3) => (4,"Am incrementat 3")
-- runWriter $ (logIncrement 3 >> logIncrement 4) => (5,"Am incrementat 3 Am incrementat 4 ")
-- runWriter $ (logIncrement 3 >>= (\x -> logIncrement (2*x))) => (9,"Am incrementat 3 Am incrementat 8 ")
-- Varianta 2
-- do
-- tell ("Am incrementat "  ++ show x)
-- return (x+1)
logIncrementN :: Int -> Int -> WriterS Int
logIncrementN x n =  do
                      tell ("Incrementam: " ++ show x)
                      if n > 1 then logIncrementN (x+1) (n-1)
                      else return (x+1)
-- Varianta 2 ma folosesc de logIncrement x
-- logIncrementN x 1 = logIncrement x
--  logIncrement x n = do 
--                        y <- logIncrementN x (n-1)
--                        logIncrement y
isPos :: Int -> WriterS Bool
isPos x = if (x>= 0) then (Writer (True, "poz")) else (Writer (False, "neg"))

mapWriterS :: (a -> WriterS b) -> [a] -> WriterS [b]
mapWriterS f [] = return []
mapWriterS f (x:xs) = do
                        y <- f x -- f x e valoarea monadica deci trebuie sa extrag din el folsind '<-'
                        ys <- mapWriterS f xs
                        return (y:ys)
{- Vezi cod pt WriterI
              modificare monada
newtype WriterI a = Writer { runWriter :: (a, Int) } --(Int, +, 0) monoid

instance  Monad WriterI where
  return va = Writer (va, 0) -- se schimba aici
  ma >>= k = let (va, log1) = runWriter ma
                 (vb, log2) = runWriter (k va)
             in  Writer (vb, log1 + log2) -- se schimba aici
Raman neschimbate Aplicative si Functor si tell
Redefinire functii de logl si loglN
logl :: String -> WriterI String
logl s = do
          tell (length s)
          return (s ++ "!")
loglN :: String -> Int -> WriterI String
loglN x l = logl x
;pglN x n = do
            y <- logl x
            loglN y (n-1)

-}
