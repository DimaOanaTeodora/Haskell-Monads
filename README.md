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
   newtype Identity a = Identity { runIdentity :: a }
   term0 = (App (Lam "x" (Var "x" :+: Var "x")) (Con 10 :+: Con 11)) => "42"
   ```
   - data Value 
   - interpretor mini Haskell for Identity Monad with data Value
   - test program
   - show function for Identity monad
   
  ### * [Maybe Monad and data Value](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/FLP45mMaybe.hs)
   
   ```
   term0 = (App (Lam "x" (Var "x" :+: Var "x")) (Con 10 :+: Con 11)) => Just 42
   ```
   - Maybe Monad (is system defined)
   - data Value
   - interpretor mini Haskell for Maybe Monad with data Value
   - test programs
   - show function (system defined for Maybe) 
   
  ### * [Either String Monad and data Value](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/FLP45mEitherString.hs)
   
   ```
   term0 = (App (Lam "x" (Var "x" :+: Var "x")) (Con 10 :+: Con 11)) => Right 42
   term1 = (App (Con 7) (Con 2)) [] => Left "should be function: 7"
   ```
   - Either String Monad (is system defined)
   - data Value
   - interpretor mini Haskell for Either String Monad with data Value
   - test programs
   - show function (system defined for Either String) 
  
  ### * [List Monad and data Value](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/FLP45mList.hs)
   
   ```
   interp (App (Lam "x" (Var "x" :+: Var "x")) (Amb (Con 1) (Con 2)))) [] => [2,4]
   ```
   - List Monad (is system defined)
   - data Value
   - interpretor mini Haskell for List Monad with data Value
   - test program
   - show function (system defined for List) 
      
  ### * [EnvReader Monad and data Value](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/FLP45mEnvReader.hs)
   
   ```
   newtype EnvReader a = Reader { runEnvReader :: Environment -> a }
   term0 = (App (Lam "x" (Var "x" :+: Var "x")) (Con 10 :+: Con 11)) => "42"
   ```
   - Reader Monad with environment 
   - interpretor function with one argument
   - ask, local - 2 auxliary functions for EnvReader
 
  ### * [Writer Monad and data Value](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/FLP3mWriterL.hs)
   
   ```
   newtype StringWriter a = StringWriter { runStringWriter :: (a, String) }
   interp (Out (Con 41) :+: Out (Con 1)) [] => "Output: 41; 1; Value: 42"
   ```
   - tell auxiliary function for output a message 
   - interpretor mini Haskell for Writer Monad with data Value
   - test program
   - show function 
   
  ### * [State Monad and data Value](https://github.com/DimaOanaTeodora/Haskell-Monads/blob/main/FLP45mState.hs)
   
   ```
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

