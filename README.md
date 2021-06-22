# Haskell Course :crystal_ball:
## Monads Labs

* [Lab 1](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/lab1.pdf)
* [Lab 2](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/lab2.pdf)
* [Lab 3](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/lab3.pdf)
* [Lab 4-5](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/lab4-5.pdf)
* [Lab 6](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/lab6.pdf)
* [Lab 7](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/lab7.pdf)

## Tests + Solutions: 

* [Var 1](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/Colocviu1.hs)
* [Var 2](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/Colocviu2.hs)
* [Var 3](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/Colocviu3.hs)
* [Var 4](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/Colocviu4.hs)
* [Var 5](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/Colocviu5.hs)
* [Var 6](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/Colocviu6.hs)
* [Var 7](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/Colocviu7.hs)

## :arrow_down: Haskell Code :arrow_down:

### :biohazard: [Class 1](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/FLP1.hs)

   - Mini-Haskell
   - Counter
   - lambda 
   - operator $
 
### :biohazard: [Class 2](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/FLP2.hs)

   - Statements
   - While 
   - Operators := and :
 
### :biohazard: Class 3
```
 >= aplica si pastreaza tipul de apel
 >> transforma 
 <=< compunere inversa
 (Just 3) >>= (\ x -> if (x>0) then Just (x*x) else Nothing) => Just 9
 (Just 3) >> (Just 6) => Just 6
 f <=< g $ 3 => Just 10 (il ia pe 3, ii aplica g, si valorii obtinute ii aplica f)
```

  * [Maybe Monad](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/FLP3mMaybe.hs)
  
      - Just, Nothing
      - transformations DO
      - tests with QuickCheck module
      - my map
      
  * [Writer String Monad](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/FLP3mWriter.hs)
  
      - log-increment functions
      
  * [Writer List Monad](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/FLP3mWriterL.hs)
  
      - log-increment functions
      - normal map
      - my map
      
  * [Reader Monad](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/FLP3mReader.hs)
  
      - show a Person (data)
      - show a Person (Reader monad)
      
### :biohazard: Class 4-5 
  ### * [Identity Monad and data Value](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/FLP45mIdentity.hs)
   
   ```
   type M = Identity
   newtype Identity a = Identity { runIdentity :: a }
   term0 = (App (Lam "x" (Var "x" :+: Var "x")) (Con 10 :+: Con 11)) => "42"
   ```
   - data Value 
   - interpretor mini Haskell for Identity Monad with data Value
   - test program
   - show function for Identity monad
   
  ### * [Maybe Monad and data Value](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/FLP45mMaybe.hs)
   
   ```
   type M = Maybe
   term0 = (App (Lam "x" (Var "x" :+: Var "x")) (Con 10 :+: Con 11)) => Just 42
   ```
   - Maybe Monad (is system defined)
   - data Value (without Wrong)
   - interpretor mini Haskell for Maybe Monad with data Value
   - test programs
   - without showM function (system defined for Maybe) 
   
  ### * [Either String Monad and data Value](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/FLP45mEitherString.hs)
   
   ```
   type M = Either String
   term0 = (App (Lam "x" (Var "x" :+: Var "x")) (Con 10 :+: Con 11)) => Right 42
   term1 = (App (Con 7) (Con 2)) [] => Left "should be function: 7"
   ```
   - Either String Monad (is system defined)
   - data Value
   - interpretor mini Haskell for Either String Monad with data Value
   - test programs
   - without showM function (system defined for Either String)
   - Left "error text" or Left (show Wrong) for errors 
  
  ### * [List Monad and data Value](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/FLP45mList.hs)
   
   ```
   -> intoarce lista raspunsurilor posibile
   type M a = [a]
   interp (App (Lam "x" (Var "x" :+: Var "x")) (Amb (Con 1) (Con 2)))) [] => [2,4]
   ```
   - List Monad (is system defined)
   - data Value
   - interpretor mini Haskell for List Monad with data Value
   - test program
   - show function (system defined for List) 
   - New: Fail and Amb constructors
      
  ### * [EnvReader Monad and data Value](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/FLP45mEnvReader.hs)
   
   ```
   type M = EnvReader
   newtype EnvReader a = Reader { runEnvReader :: Environment -> a }
   term0 = (App (Lam "x" (Var "x" :+: Var "x")) (Con 10 :+: Con 11)) => "42"
   ```
   - Reader Monad with environment 
   - interpretor function with one argument (it contains the environment)
   - ask, local - 2 auxliary functions for EnvReader
 
  ### * [Writer Monad and data Value](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/FLP3mWriterL.hs)
   
   ```
   type M = StringWriter
   newtype StringWriter a = StringWriter { runStringWriter :: (a, String) }
   interp (Out (Con 41) :+: Out (Con 1)) [] => "Output: 41; 1; Value: 42"
   ```
   - tell auxiliary function for output a message 
   - interpretor mini Haskell for Writer Monad with data Value
   - test program
   - show function 
   
  ### * [State Monad and data Value](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/FLP45mState.hs)
   
   ```
   type M = InState
   newtype IntState a = IntState { runIntState :: Integer -> (a, Integer) }
   interp ((Con 1 :+: Con 2) :+: Count) [] => "Value: 4; Count: 2"
   ```
   - modify, get auxiliary functions 
   - interpretor mini Haskell for State Monad with data Value
   - test program
   - show function 
      
### :biohazard: Class 6

   * [Module SIMPLE](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/FLP6SIMPLE.hs)
   * [Simple Type Checker](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/FLP6.hs)
   
### :biohazard: Class 7

   * [Module SIMPLE](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/FLP6SIMPLE.hs)
   * [Interpretor SIMPLE Module](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/FLP7.hs)

