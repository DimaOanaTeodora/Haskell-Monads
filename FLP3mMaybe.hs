import Test.QuickCheck
{- Monada Maybe este definita in GHC.Base 

instance Monad Maybe where
  return = Just
  Just va  >>= k   = k va
  Nothing >>= _   = Nothing


instance Applicative Maybe where
  pure = return
  mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)       

instance Functor Maybe where              
  fmap f ma = pure f <*> ma   
-}

(<=<) :: (a -> Maybe b) -> (c -> Maybe a) -> c -> Maybe b
f <=< g = (\ x -> g x >>= f)

f,g,h ::Int -> Maybe Int
f = \x->if (x>0) then Just (x+3) else Nothing
g = \x-> if (x>2) then Just(x-3) else Nothing
h = \x-> if (x<2) then Just (x+x) else Nothing
-- f<=< g $ 3 => Nothing
-- f<=< g $ 4 => Just 4
ex = (\x -> if x>'a' then Just 1 else Nothing) <=< (\x-> if x>0 then Just ('a') else Nothing)
--ex $ 3 => Nothing ca-l face mereu pe g (al doilea)

--prorietatea de asociativitate pt compunerea functiilor
asoc :: (Int -> Maybe Int) -> (Int -> Maybe Int) -> (Int -> Maybe Int) -> Int -> Bool
asoc f g h x = (h <=< (g <=< f) $ x) ==((h <=< g) <=< f $ x)
-- *Main> quickCheck (asoc f g h)
-- +++ OK, passed 100 tests.

--comutativitate nu e pt compunerea functiilor
comm :: (Int -> Maybe Int) -> (Int -> Maybe Int) ->  Int -> Bool
comm f g x = ((g <=< f) $ x )==((f <=< g) $ x)
-- quickCheck (comm f g)
-- *** Failed! Falsified (after 2 tests): 1

pos :: Int -> Bool
pos  x = if (x>=0) then True else False

foo :: Maybe Int ->  Maybe Bool 
foo  mx =  mx  >>= (\x -> return (pos x))  
-- foo (Just 3) => Just True
-- foo nothing => Nothing

fooDo :: Maybe Int ->  Maybe Bool  
fooDo mx=do
            x <- mx --scot valoarea din cutie
            return (pos x) --il pun in cutie


addM1 :: Maybe Int -> Maybe Int -> Maybe Int  
addM1 mx my = do
                x<-mx
                y<-my
                return (x+y) --merge si Just(x+y)
--addM (Just 3) (Just 2) => Just 5

addM2 :: Maybe Int -> Maybe Int -> Maybe Int  
addM2 Nothing _ = Nothing
addM2 _ Nothing = Nothing
addM2 (Just x)(Just y)=Just (x+y)

addM3 :: Maybe Int -> Maybe Int -> Maybe Int 
addM3 mx my = mx >>= (\x -> ( my >>= (\y -> return (x+y)) ) ) 

prop a b = addM1 a b == addM2 a b && addM2 a b == addM3 a b
-- *Main> quickCheck prop
-- +++ OK, passed 100 tests.

cp xs ys = xs >>= ( \x -> (ys >>= \y-> return (x,y)))
cpdo :: Monad m => m a -> m b -> m(a,b)
cpdo xs ys =do
              x<-xs
              y<-ys
              return (x,y)
--cpdo (Just 3) (Just 4) => Just (3,4)
--cpdo [1,2] [3,4] -> [(1,3), (1,4), (2,3), (2,4)]

prod f xs ys = [f x y | x <- xs, y<-ys]
prod1 f xs ys = xs >>= \x -> [f x y | y <- ys]
-- *Main> prod1 (+) [1,2] [3,4]
-- [4,5,5,6]
prod2 f xs ys = xs >>= \x ->( ys >>= \y -> return (f x y ))

myGetLine :: IO String
myGetLine = getChar >>= \x -> if x == '\n' then return [] else myGetLine >>= \xs -> return (x:xs)

myGetLinedo :: IO String
myGetLinedo = do 
                x<- getChar
                if x== '\n' then return [] else
                  do
                    xs <- myGetLinedo
                    return (x:xs)
-- *Main> myGetLinedo
-- ionel
-- "ionel"

prelNo noin = sqrt noin
ioNumber = do
            noin <- readLn :: IO Float
            putStrLn $ "Intrare\n" ++ (show noin)
            let noout = prelNo noin
            putStrLn $ "Iesire"
            print noout
ioNumberM =(readLn :: IO Float) >>=
              \noin -> putStrLn ("Intrare\n" ++ show noin) >>
                let noout = prelNo noin in putStrLn "Iesire" >> print noout
-- *Main> ioNumberM
--2
--Intrare
--2.0
--Iesire
--1.4142135