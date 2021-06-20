import Test.QuickCheck
-- >= aplica si pastreaza tipul de apel
-- >> transforma 
-- (Just 3) >>= (\ x -> if (x>0) then Just (x*x) else Nothing) => Just 9
-- (Just 3) >> (Just 6) => Just 6
-- <=< compunere inversa
-- f <=< g $ 3 => Just 10 (il ia pe 3, ii aplica g, si valorii obtinute ii aplica f)

-- Functii peste testare cu QuickCheck
f :: Int -> Maybe Int
f x = if x >2 then Just (x+1) else Nothing
g :: Int -> Maybe Int
g x = if x>0 then Just (x*x) else Nothing
h :: Int -> Maybe Int
h x = if x<0 then Just (x `div` 2) else Nothing

(<=<) :: (a -> Maybe b) -> (c -> Maybe a) -> c -> Maybe b
f <=< g = (\ x -> g x >>= f)

asoc :: (Int -> Maybe Int) -> (Int -> Maybe Int) -> (Int -> Maybe Int) -> Int -> Bool
asoc f g h x = ((f <=< g) <=< h $ x) == (f <=< (g <=< h) $ x )
-- Testare proprietate : quickCheck (asoc f g h)

pos :: Int -> Bool
pos  x = if (x>=0) then True else False

foo :: Maybe Int ->  Maybe Bool 
foo  mx =  mx  >>= (\x -> Just (pos x)) 

-- Notatia DO ------
fooDo :: Maybe Int ->  Maybe Bool
fooDo mx = do
      x <- mx
      return (pos x)

addMSabloane :: Maybe Int -> Maybe Int -> Maybe Int  
addMSabloane (Just x) (Just y) = Just (x+y)
addMSabloane _ _ = Nothing

addMDo :: Maybe Int -> Maybe Int -> Maybe Int
addMDo x y = do
      a <- x -- din x = (Just a) scot a
      b <- y -- din y = (Just b) scot b
      return (a+b)  -- din (a+b) fac Just(a+b)

test :: Maybe Int -> Maybe Int -> Bool
test x y = addMDo x y == addMSabloane x y
-- quickCheck test

-- Tranformare functii normale in notatie do ---
-- TIP: operatorul >>= se inlocuieste cu <- in do
cartesian_product :: [a] -> [b] -> [(a,b)]
cartesian_product xs ys = xs >>= ( \x -> (ys >>= \y-> return (x,y)))

cpDo :: [a] -> [b] -> [(a,b)]
cpDo xs ys = do
      x <- xs
      y <- ys
      return (x,y)

prod f xs ys = [f x y | x <- xs, y<-ys] -- fiecare elem din xs cu toate din ys

-- functie pentru test
func x y = x + y

prodDo f xs ys = do
      x <- xs
      y <- ys
      return (f x y) -- intoarce automat lista pt ca List e monada

-- TIP: operatorul >>= se inlocuieste cu <- in do
      -- nume1 >>= \nume2 => nume2 <- nume1
myGetLine :: IO String
myGetLine = getChar >>= \x -> if x == '\n' then return [] else myGetLine >>= \xs -> return (x:xs)

getDo :: IO String
getDo = do
      x <- getChar
      if x == '\n' then 
            return [] 
            else 
                  do
                        xs<- getDo
                        return (x:xs)

-- Transformare din DO in functii normale --
-- TIP: >> operator de legatura instructiunilor pentru transformarea din DO
prelNo noin = sqrt noin

ioNumber = do
      noin <- readLn :: IO Float
      putStrLn $ "Intrare\n" ++ (show noin)
      let noout = prelNo noin
      putStrLn $ "Iesire"
      print noout

ioDo =(readLn :: IO Float) >>=
              \noin -> putStrLn ("Intrare\n" ++ show noin) >>
                let noout = prelNo noin in putStrLn "Iesire" >> print noout

--- MAP cu monada Maybe ---
mymap :: (a -> Maybe b) -> [a] -> Maybe [b]
mymap f [] = return []
mymap f (x:xs) = do
                  y <- f x 
                  ys <- mymap f xs
                  return (y:ys)
