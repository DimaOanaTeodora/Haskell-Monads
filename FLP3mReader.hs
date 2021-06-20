---Monada Reader
newtype Reader env a = Reader { runReader :: env -> a }

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


ask :: Reader env env -- functie auxiliara
ask = Reader id

--- fara monade -----
data Person = Person { name :: String, age :: Int }

showPersonN :: Person -> String
showPersonN (Person name age) = "Nume: " ++ name

showPersonA :: Person -> String
showPersonA (Person name age) = "Varsta: " ++ show age

showPerson :: Person -> String
showPerson person =  (showPersonN person) ++ ", " ++ (showPersonA person)
-- showPerson (Person "Ionel" 12) SAU showPerson $ Person "ada" 20   

---- cu monada Reader -----
mshowPersonN ::  Reader Person String
mshowPersonN = do
      p <- ask --  extrag Person din Reader Person
      return ("Nume: " ++ name p)

mshowPersonA ::  Reader Person String
mshowPersonA = do
      p <- ask --  extrag Person din Reader Person
      return ("Varsta: " ++ show (age p)) -- le bag in monada iar prin return

mshowPerson :: Reader Person String
mshowPerson = do
      nume <- mshowPersonN -- trebuie scoase din monadele din cauza ca le-am intors in return
      varsta <- mshowPersonA
      return (nume ++ "," ++ varsta)
-- runReader mshowPerson $ Person "ada" 20
