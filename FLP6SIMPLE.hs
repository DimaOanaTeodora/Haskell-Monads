module FLP6SIMPLE where

type Name = String

data BinAop = Add | Mul | Sub | Div | Mod

instance Show BinAop where
    show Add = "+"
    show Mul = "*"
    show Sub = "-"
    show Div = "/"
    show Mod = "%"

data BinCop = Lt | Lte | Gt | Gte

instance Show BinCop where
    show Lt = "<"
    show Lte = "<="
    show Gt = ">"
    show Gte = ">="

data BinEop = Eq | Neq

instance Show  BinEop where
    show Eq = "=="
    show Neq = "!="

data BinLop = And | Or

instance Show BinLop where
    show And = "&&"
    show Or = "||"

data Exp
    = Id Name
    | I Integer
    | B Bool
    | UMin Exp
    | BinA BinAop Exp Exp       -- BinA Add e1 e2 BinaA Mod e1 e3 
    | BinC BinCop Exp Exp       -- nu e corecta: Add e1 e2
    | BinE BinEop Exp Exp
    | BinL BinLop Exp Exp
    | Not Exp

instance Show Exp where
    show (Id x) = x
    show (I i) = show i
    show (B True) = "true"
    show (B False) = "false"
    show (UMin e) = "-" <> show e
    show (BinA op e1 e2) = addParens $ show e1 <> show op <> show e2
    show (BinC op e1 e2) = show e1 <> show op <> show e2
    show (BinE op e1 e2) = show e1 <> show op <> show e2
    show (BinL op e1 e2) = addParens $ show e1 <> show op <> show e2
    show (Not e) = "!" <> show e

addParens :: String -> String
addParens e = "(" <> e <> ")"


data Stmt
    = Asgn Name Exp
    | If Exp Stmt Stmt
    | Read String Name
    | Print String Exp
    | While Exp Stmt
    | Block [Stmt]
    | Decl Name Exp
  deriving (Show)
  
-- doua programele  
pFact= Block [ 
       Decl "n" (I 5),
       Decl "fact " (Id "n"),
       Decl "i" (I 1),
       While (BinE Neq  (Id "n") (Id "i")) 
                (Block [ Asgn "fact" (BinA Mul (Id "fact") (Id "i")),
                       Asgn "i" (BinA Add (Id "i") (I 1))
                       ])
              ]  
-- e gresit dar nu ca sintaxa
pFactW =Block [ 
       Asgn "n" (I 5),
       Asgn "fact " (Id "n"),
       Asgn "i" (I 1),
       While (BinE Neq  (Id "n") (Id "i")) 
                (Block [ Asgn "fact" (BinA Mul (Id "fact") (Id "i")),
                       Asgn "i" (BinA Add (Id "i") (I 1))
                       ])
              ]
