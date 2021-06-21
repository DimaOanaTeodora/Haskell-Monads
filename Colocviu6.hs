{-
Sandu Razvan Alexandru 241
 
Gasiti mai jos limbajul unui minicalculator si o interpretare partiala.
Calculatorul are o celulă de memorie, care are valoarea initiala  0.
Un program este o expresie de tip `Prog`iar rezultatul executiei este lista valorilor calculate.
Testare se face apeland `prog test`.
-}
data Prog = On Stmt
 
data Stmt
  = Off
  | Save Expr Stmt -- evalueaza expresia și salvează rezultatul in Mem, apoi evalueaza Stmt
  | NoSave Expr Stmt -- evalueaza expresia, fără a modifica Mem, apoi evaluează Stmt
 
data Expr = Mem | V Int | Expr :+ Expr | Expr :* Expr | If Expr Expr
 
infixl 6 :+
infixl 7 :*
type Env = Int -- valoarea curentă a celulei de memorie
 
expr :: Expr -> Env -> Int
expr (e1 :+ e2) m = expr e1 m + expr e2 m
expr (e1 :* e2) m = expr e1 m * expr e2 m
expr (V x) m = x
expr Mem m = m
expr (If e1 e2) m = if m == 0 then expr e1 m else expr e2 m
 
stmt :: Stmt -> Env -> [Int]
stmt Off _ = []
stmt (Save e s) m = let it = expr e m in it : stmt s it
stmt (NoSave e s) m = expr e m : stmt s m
 
prog :: Prog -> [Int]
prog (On s) = stmt s 0
 
test1 :: Prog
test1 = On (Save (V 3) (NoSave (Mem :+ V 5) Off))
 
test2 :: Prog
test2 = On (NoSave (V 3 :+ V 3) Off)
 
ifTest2 :: Prog
ifTest2 = On (Save (If (V 1) (V 2)) Off) -- memorie goala
 
ifTest3 :: Prog
ifTest3 = On (Save (V 1) (NoSave (V 2) (Save (V 3) (Save (If (V 4) (Mem :* V 5)) Off)))) -- memoria != 0
-- deoarace avem 2 in Mem                                             ^ se evalueaza asta
 
 
 
 
{-CERINTE
 
1) (10pct) Finalizati definitia functiilor de interpretare.
2) (10 pct) Adaugati expresia `If e1 e2` care se evaluează `e1` daca `Mem` are valoarea `0` si la `e2` in caz contrar.
3) (20pct) Definiti interpretarea  limbajului extins astfel incat executia unui program sa calculează valoarea finala,
numarul de adunari si numarul de inmultiri efectuate.
Rezolvați subiectul 3) în același fișier redenumind funcțiile de interpretare.
 
Indicati testele pe care le-ati folosit in verificarea solutiilor.
 
-}
