module Checker where


import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import FLP6SIMPLE

data Type = TInt | TBool
  deriving (Eq)

instance Show Type where
    show TInt = "int"
    show TBool = "bool"

type CheckerState = Map Name Type

emptyCheckerState :: CheckerState
emptyCheckerState = Map.empty

newtype EReader a =
    EReader { runEReader :: CheckerState ->  (Either String a) }

throwError :: String -> EReader a
throwError e = EReader (\_ -> (Left e))

instance Monad EReader where
    return a = EReader (\env ->  Right a)
    act >>= k = EReader  f
                where
                 f env  = case (runEReader act env) of
                           Left s -> Left s
                           Right va -> runEReader (k va) env


instance Functor EReader where
    fmap f ma = do { a <- ma; return (f a) }

instance Applicative EReader where
    pure = return
    mf <*> ma = do { f <- mf; a <- ma; return (f a)}

askEReader :: EReader CheckerState
askEReader =EReader (\env -> Right env)

localEReader :: (CheckerState -> CheckerState) -> EReader a -> EReader a
localEReader f ma = EReader (\env -> (runEReader ma) (f env))


type M = EReader

expect :: (Show t, Eq t, Show e) => t -> t -> e -> M ()
expect tExpect tActual e =
    if (tExpect /= tActual)
    then     (throwError
        $ "Type mismatch. Expected " ++ show tExpect ++ " but got " ++ show tActual
        ++ " for " ++ show e)
    else (return ())

lookupM :: Name -> M Type
lookupM x =do
            env <- askEReader -- imi da mediul de evaluare
            case Map.lookup x env of --ma folosesc de lookup din Map
                Just val -> return val
                Nothing -> throwError ("Variabila " ++ x ++ " nu a fost declarata") 
            
checkExp :: Exp -> M Type
checkExp (I n)= return TInt
checkExp (B b)= return TBool
checkExp (Id x)= lookupM x
checkExp (UMin e)= do
                    te <- checkExp e
                    expect TInt te e
                    return TInt  
checkExp (Not e)= do
                    te <- checkExp e
                    expect TBool te e
                    return TBool 
checkExp (BinA op e1 e2) = do
                              t1 <- checkExp e1
                              expect TInt t1 e1
                              t2 <- checkExp e2
                              expect TInt t2 e2
                              return TInt 
checkExp (BinC _ e1 e2) = do
                              t1 <- checkExp e1
                              expect TInt t1 e1
                              t2 <- checkExp e2
                              expect TInt t2 e2
                              return TBool
checkExp (BinE _ e1 e2) = do
                              t1 <- checkExp e1
                              t2 <- checkExp e2
                              expect t1 t2 e2
                              return TBool  
-- runEReader (checkExp (I 3)) $ emptyCheckerState => Right int
-- runEReader (checkExp (I 3)) $ Map.insert "x" TInt emptyCheckerState => Right int
-- runEReader (checkExp (UMin (Id "y"))) $ Map.insert "y" TBool emptyCheckerState => Left "Type mismatch. Expected int but got bool for y"

checkStmt :: Stmt -> M ()
checkStmt (Block sts) = checkBlock sts
checkStmt (Decl _ _ ) = return ()
checkStmt (Asgn x e ) = do
                            tx <- lookupM x 
                            te <- checkExp e
                            expect tx te e
checkStmt (Print s e) = do
                            te <- checkExp e
                            expect TInt te e
checkStmt (While e s) = do
                            te <- checkExp e
                            expect TBool te e
                            checkStmt s
checkBlock :: [Stmt] -> M ()
checkBlock [] = return ()
checkBlock ((Decl x e): sts)= do
                                t <- checkExp e
                                localEReader (Map.insert x t) (checkBlock sts)
checkBlock (st: sts)= checkStmt st >> checkBlock sts


checkPgm :: [Stmt] -> Bool
checkPgm pgm =
    case  (runEReader (checkBlock pgm)) emptyCheckerState of
        Left err -> error err
        Right _ -> True
pgm = Block [ Decl "x" (I 3), Asgn "x" (BinC Lt (Id "y") (I 5))]
