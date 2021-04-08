
---Monada Reader


newtype Reader env a = Reader { runReader :: env -> a }
-- runReader :: Reader env a -> (env -> a)
-- aici runReader :: Reader Person String -> (Person -> String) 
-- runReader mp :: Person -> String 

instance Monad (Reader env) where
  return x = Reader (\_ -> x)
  ma >>= k = Reader f
    where f env = let a = runReader ma env
                  in  runReader (k a) env



instance Applicative (Reader env) where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)

instance Functor (Reader env) where
  fmap f ma = pure f <*> ma


ask :: Reader env env
ask = Reader id

local :: (env -> env) -> Reader env a -> Reader env a
local f ma = Reader $ (\env -> (runReader ma)(f env))

-- Reader Person String

data Person = Person { name :: String, age :: Int }

showPersonN :: Person -> String
showPersonN (Person name age )= "NAME: " ++ name

showPersonA :: Person -> String
showPersonA (Person name age ) = "AGE: " ++ show (age)


showPerson :: Person -> String
showPerson p = "(" ++ showPersonN p ++ "," ++ showPersonA p ++ ")"
-- showPerson (Person "A" 10) => "(NAME: A,AGE: 10)"


-- Asta e o functie deci trebuie sa fac runReader mshowPersonN $
mshowPersonN ::  Reader Person String 
mshowPersonN = do
               p <- ask           
               return ("NAME: " ++ (name p))
-- (runReader mshowPersonN) $ (Person "A" 10) => "NAME: A"

{- varianta fara scriere monadica
mshowPersonN ::  Reader Person String --Reader f unde f :: Person -> String
mshowPersonN = Reader f               --runReader mshowPersonN -> f
                where 
                  f (Person n a) = "NAME: " ++ n
-}
mshowPersonA ::  Reader Person String
mshowPersonA = Reader f
                where 
                  f (Person n a) =  "AGE: " ++ show (a)

mshowPerson :: Reader Person String
mshowPerson = do
                n <- mshowPersonN 
                a <- mshowPersonA 
                return ("(" ++ n ++ "," ++ a ++ ")")
--(runReader mshowPerson) $ (Person "A" 10) => "(NAME: A,AGE: 10)"