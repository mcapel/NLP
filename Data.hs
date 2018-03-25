module Data 
where

import Char
import qualified Data.Map as M
import qualified Data.Set as S
import List hiding (delete)

--------------------------------------------
-- Connectives
--------------------------------------------
data C = L | P | R | X
    deriving (Eq,Ord)

instance Show C where
  show R = "/"
  show L = "\\"
  show P = "*"
  show X = "X"
--------------------------------------------
-- Formulas
--------------------------------------------
data F = S
        | A String -- atoms 
        | B C F F  -- formulas
    deriving (Eq,Ord)

instance Show F where
  show (A x) = mShowString x
  show S = "S"
  show (B c a b) = (("(" ++) . (show a ++) . (show c ++) . (show b ++) . (")" ++)) ""
  
a = A "a"
b = A "b"
c = A "c"
s = A "s"
n = A "n"

infixr 4  *>
infixl 4  <*
infix 3 |*|, |.|

(<*) :: F -> F -> F
a <* b = B R a b

(*>) :: F -> F -> F
a *> b = B L a b

(|*|) :: F -> F -> F
a |*| b = B P a b

(|.|) :: F -> F -> F
a |.| b = B X a b

conn :: F -> C
conn (B c _ _) = c

degree :: F -> Int
degree (A _) = 0
degree (B _ a b) = 1 + degree a + degree b

degrees :: [F] -> Int
degrees [] = 0
degrees (x:xs) = degree x + degrees xs

subform :: F -> F -> Bool
subform x y@(A _) = x==y
subform x y@(B _ a b) = x==y || subform x a || subform x b

inner :: F -> F
inner (B R _ (B L a _)) = a
inner (B L (B R _ a) _) = a
inner (B L _ (B p _ a)) = a
inner (B R (B P a _) _) = a
inner (B P _ (B L _ a)) = a
inner (B P (B R a _) _) = a
--------------------------------------------
-- Lexicon
--------------------------------------------
type Lex = [(String,F,Lam')]

type Lex' = [(Lam',CB,String,F,F)]

showAss :: (Lam',CB,String,F,F) -> ShowS
showAss (t,_,w,a,c) = 
    ("${\\scriptscriptstyle " ++) . latexS t . 
    ("}$ & " ++) . (mShowString w ++) . 
    (" & ${\\scriptscriptstyle " ++) . latexS a . 
    ("}$ & ${\\scriptscriptstyle " ++) . latexS c . ("}$\\\\[.2cm]\n" ++)

showAss' :: (String,F,Lam') -> ShowS
showAss' (w,a,t) = 
    ("${\\scriptscriptstyle " ++) . latexS t . 
    ("}$ & " ++) . (mShowString w ++) . 
    (" & ${\\scriptscriptstyle " ++) . latexS a . ("}$\\\\[.2cm]\n" ++)

lexTab :: Lex' -> ShowS
lexTab xs =
    ("\\begin{longtable}{l@{:}l@{:}l@{$\\quad\\to\\quad$}l}\n" ++) . 
    (concatMap (\ x -> showAss x "") xs ++) . 
    ("\\end{longtable}" ++)

lexTab' :: Lex -> ShowS
lexTab' xs =
    ("\\begin{longtable}{l@{\\;::\\;}l@{\\;::\\;}l}\n" ++) . 
    (concatMap (\ x -> showAss' x "") xs ++) . 
    ("\\end{longtable}" ++)

latexTests :: [(String,F)] -> ShowS
latexTests xs =
    ("\\begin{longtable}{l@{$\\;\\to\\;$}l}\n" ++) . 
    latexTest xs .
    ("\\end{longtable}" ++)

latexTest :: [(String,F)] -> ShowS
latexTest [] = ("" ++)
latexTest ((x,y):xs) = 
    (mShowString x ++) . (" & $" ++) .
    latexS y . ("$\\\\[.2cm]\n" ++) .
    latexTest xs

wrapLex :: Lex -> [(String,F)] -> ShowS
wrapLex lex tests = 
    ("\\documentclass[10pt]{article}\n" ++) .
    ("\\usepackage{amsmath,mathrsfs}\n" ++) .
    ("\\usepackage{a4wide}\n" ++) .
    ("\\usepackage{graphicx}\n" ++) .
    ("\\usepackage{stmaryrd}\n" ++) .
    ("\\usepackage{proof}\n" ++) .
    ("\\usepackage{textcomp}\n" ++) .
    ("\\usepackage{ifthen}\n" ++) .
    ("\\usepackage{color}\n" ++) .
    ("\\usepackage{calc}\n" ++) .
    ("\\usepackage{latexsym}\n\n" ++) .
    ("\\usepackage{longtable}" ++) . 
    ("\\pagestyle{empty}\n" ++) .
    ("\\definecolor{azzurro}{rgb}{0.1,0.2,0.8}\n" ++) .
    ("\\newcommand{\\textazz}[1]{\\mbox{\\scriptsize{\\textcolor{azzurro}{$#1$}}}}\n" ++) .
    ("\\newcommand{\\Boxd}{\\Box}\n" ++) .
    ("\\newcommand{\\bs}{\\backslash}\n" ++) .
    ("\\newcommand{\\bo}{[}\n" ++) .
    ("\\newcommand{\\bb}{]}\n" ++) .
    ("\\newcommand{\\dia}{\\diamondsuit}\n" ++) .
    ("\\newcommand{\\lx}{\\vartriangleleft}\n" ++) .
    ("\\newcommand{\\ev}{\\text{ev}}\n" ++) .
    ("\\newcommand{\\rx}{\\vartriangleright}\n" ++) .
    ("\\setlength{\\textheight}{22cm}\n" ++) .
    ("\\setlength{\\textwidth}{18cm}\n" ++) .
    ("\\setlength{\\topmargin}{0cm}\n" ++) .
    ("\\setlength{\\oddsidemargin}{-1cm}\n" ++) .
    ("\\setlength{\\evensidemargin}{-1cm}\n" ++) .
    ("\\begin{document}\n" ++) .
    ("\\section{Lexicon}\n" ++) .
    lexTab' lex .
    ("\\section{Tests}\n" ++) .
    latexTests tests .
    ("\\end{document}\n\n" ++)
    
writeLex :: String -> IO ()
writeLex t = writeFile ("Tex/lex.tex") t
--------------------------------------------
-- Proof terms
--------------------------------------------
data CB = I F | H F
              | M C (CB,CB)  | CB Char C 
              | Cp (CB,CB)   | Sh C
              | Ap C (CB,CB) | KB Char C (F,F)
    deriving (Eq,Ord)

instance Show CB where
  show (I a) = "1" ++ show a
  show (H a) = "1" ++ show a
  show (M c a) = show c ++ show a
  show (CB x c) = show x
  show (Cp (x,y)) = show x ++ "." ++ show y
  show (Sh x) = show x
  show (Ap x c) = "AP" ++ show x ++ show c
  show (KB c d x) = show c ++ show x

-- Basic combinators
cop c f = Cp (f,CB 'c' c) -- Co-application
app c f = Cp (CB 'a' c,f) -- Application
lif c f = Cp (f,CB 'l' c) -- Lifting
--------------------------------------------
-- From Proof Terms to Proof Trees
--------------------------------------------
cb2ty :: CB -> (F,F)
cb2ty (I a) = (a,a)
cb2ty (M R (f,g)) =
    (B R a b,B R a' b')
            where 
               (a,a') = cb2ty f
               (b',b) = cb2ty g
cb2ty (M L (f,g)) =
    (B L b a,B L b' a')
            where 
               (a,a') = cb2ty f
               (b',b) = cb2ty g
cb2ty (M P (f,g)) =
    (B P a b,B P a' b')
            where 
               (a,a') = cb2ty f
               (b,b') = cb2ty g
cb2ty (M X (f,g)) =
    (B X a b,B P a' b')
            where 
               (a,a') = cb2ty f
               (b,b') = cb2ty g
cb2ty (Cp (f,CB 'l' R)) =
        (a,b)
            where 
               (B R _ (B L a _),b) = cb2ty f
cb2ty (Cp (f,CB 'l' L)) =
        (a,b)
            where 
               (B L (B R _ a) _,b) = cb2ty f
cb2ty (Cp (f,CB 'c' R)) =
        (a,b)
            where 
               (B R _ (B P _ a),b) = cb2ty f
cb2ty (Cp (f,CB 'c' L)) =
        (a,b)
            where 
               (B L (B P a _) _,b) = cb2ty f
cb2ty (Cp (CB 'a' R,f)) =
        (a,b)
            where 
               (a,B P (B R b _) _) = cb2ty f
cb2ty (Cp (CB 'c' L,f)) =
        (a,b)
            where 
               (a,B P _ (B L _ b)) = cb2ty f
cb2ty (Cp (f,g)) =
        (a,b) 
            where 
              (a,_) = cb2ty g
              (_,b) = cb2ty f

cb2pr :: CB -> Proof
cb2pr g@(I a) = Lf (g,a,a)
cb2pr g@(KB 'a' L (a,b)) = Lf (g,b|*|b*>a,a)
cb2pr g@(KB 'a' R (a,b)) = Lf (g,a<*b|*|b,a)
cb2pr g@(KB 'c' L (a,b)) = Lf (g,a,b*>(b|*|a))
cb2pr g@(KB 'c' R (a,b)) = Lf (g,a,(a|*|b)<*b)
cb2pr g@(KB 'l' L (b,a)) = Lf (g,b,(a<*b)*>a)
cb2pr g@(KB 'l' R (b,a)) = Lf (g,b,a<*(b*>a))
cb2pr g@(M L (f,h)) = Br (g,B L b a,B L b' a') l r
        where 
           l = cb2pr f
           (_,a,a') = root l
           r = cb2pr h
           (_,b',b) = root r
cb2pr g@(M R (f,h)) = Br (g,B R a b,B R a' b') l r
        where 
           l = cb2pr f
           (_,a,a') = root l
           r = cb2pr h
           (_,b',b) = root r
cb2pr g@(M P (f,h)) = Br (g,B P a b,B P a' b') l r
        where 
           l = cb2pr f
           (_,a,a') = root l
           r = cb2pr h
           (_,b,b') = root r
cb2pr g@(M X (f,h)) = Br (g,a|.|b,B P a' b') l r
        where 
           l = cb2pr f
           (_,a,a') = root l
           r = cb2pr h
           (_,b,b') = root r
cb2pr g@(Cp (CB 'a' _,f)) = Un (g,a,b) c
        where 
           c = cb2pr f
           (_,a,b') = root c
           b = inner b'
cb2pr g@(Cp (f,CB _ _)) = Un (g,a,b) c
        where 
           c = cb2pr f
           (_,a',b) = root c
           a = inner a'
cb2pr g@(Cp (f,h)) = Br (g,a,c) l r
        where
           r = cb2pr f
           l = cb2pr h
           (_,a,_) = root l
           (_,_,c) = root r
cb2pr g@(Ap R (f,h)) = Br (g,a|.|a',c) l r
        where
          l = cb2pr f
          r = cb2pr h
          (_,a,B R c _) = root l
          (_,a',_) = root r
cb2pr g@(Ap L (f,h)) = Br (g,a'|.|a,c) l r
        where
          r = cb2pr f
          l = cb2pr h
          (_,a,B L _ c) = root r
          (_,a',_) = root l
--------------------------------------------
-- Eta compact terms
--------------------------------------------
etaComp :: CB -> CB
etaComp (M L (f,g)) =
    let 
      f' = etaComp f 
      g' = etaComp g    
    in case (f',g') of
        (I a,I b) -> I (b*>a)
        _ -> M L (f',g')
etaComp (M X (f,g)) =
        M X (etaComp f,etaComp g)
etaComp (M c (f,g)) =
    let 
      f' = etaComp f 
      g' = etaComp g    
    in case (f',g') of
        (I a,I b) -> I (B c a b)
        _ -> M c (f',g')
etaComp (Ap c (f,g)) = Ap c (etaComp f,etaComp g)
etaComp (Cp (f,g)) = 
    let 
      f' = etaComp f 
      g' = etaComp g    
    in etaComp' (Cp (f',g'))
etaComp f = f

etaComp' (Cp (CB 'a' L,I (B P a (B L _ b)))) = KB 'a' L (b,a)
etaComp' (Cp (CB 'a' R,I (B P (B R b _) a))) = KB 'a' R (b,a)
etaComp' (Cp (I (B L a (B P _ b)),CB 'c' L)) = KB 'c' L (b,a)
etaComp' (Cp (I (B R (B P b _) a),CB 'c' R)) = KB 'c' R (b,a)
etaComp' (Cp (I (B L (B R _ b) a),CB 'l' L)) = KB 'l' L (b,a)
etaComp' (Cp (I (B R a (B L b _)),CB 'l' R)) = KB 'l' R (b,a)
etaComp' (Cp (I a,g)) = g
etaComp' (Cp (f,I a)) = f
etaComp' f = f
--------------------------------------------
-- Lambda terms
--------------------------------------------
data Lam' = C' String | V' Int 
         | Abs' Int Lam' | App' Lam' Lam' 
         | Pr' Lam' Lam' | Pi1' Lam' | Pi2' Lam'
         | Ex' Lam' | All' Lam' 
         | And' Lam' Lam' | Or' Lam' Lam' 
         | Imp' Lam' Lam' | Not' Lam'
        deriving (Ord,Eq)

instance Show Lam' where
    show = showLam'
    
showLam' (C' xs) = mShowString xs
showLam' (V' i) = "x" ++ show i
showLam' (Abs' i f) = 
    "\\x" ++ show i ++ "." ++ showLam' f
showLam' (Pr' x y) = "<" ++ showLam' x ++ "," ++ showLam' y ++ ">"
showLam' (Pi1' t) = 
    "pi" ++ showLam' t
showLam' (Pi2' t) = 
    "pi'" ++ showLam' t
showLam' (App' x y) = "(" ++ showLam' x ++ " " ++ showLam' y ++ ")"
showLam' (Ex' f) = 
    "[Ex" ++ showLam' f ++ "]"
showLam' (All' f) = 
    "[All" ++ showLam' f ++ "]"
showLam' (And' t t') = 
    "[" ++ showLam' t ++ "&" ++ showLam' t' ++ "]"
showLam' (Or' t t') = 
    "[" ++ showLam' t ++ "v" ++ showLam' t' ++ "]"
showLam' (Imp' t t') = 
    "[" ++ showLam' t ++ "=>" ++ showLam' t' ++ "]"
showLam' (Not' t) = 
    "[-" ++ showLam' t ++ "]"

type Env = M.Map Int Lam

reduce :: Lam' -> Lam
reduce = interp M.empty 

interp :: Env -> Lam' -> Lam
interp e (V' i) = 
    case M.lookup i e of 
        Nothing -> V i
        Just t -> t
interp _ (C' c) = C c
interp e (Abs' i t) = Abs (\ x -> interp (M.insert i x e) t)
interp e (App' tf ta) = 
    tf' $: ta' 
      where
        tf' = interp e tf
        ta' = interp e ta
interp e (Pr' t1 t2) = Pr (interp e t1) (interp e t2)
interp e (Pi1' t) = pi1 $ interp e t 
interp e (Pi2' t) = pi2 $ interp e t
interp e (Not' t) = Not $ interp e t
interp e (Ex' t) = Ex $ interp e t
interp e (All' t) = All $ interp e t
interp e (And' t1 t2) = And (interp e t1) (interp e t2)
interp e (Or' t1 t2) = Or (interp e t1) (interp e t2)
interp e (Imp' t1 t2) = Imp (interp e t1) (interp e t2)

frees0 :: Lam' -> S.Set Int
frees0 t = frees' t S.empty

frees' :: Lam' -> S.Set Int -> S.Set Int
frees' (V' i) _ = S.singleton i
frees' (C' _) _ = S.empty
frees' (Abs' i t) is = S.delete i (frees' t is)
frees' (All' x) is = frees' x is
frees' (Ex' x) is = frees' x is
frees' (Not' x) is = frees' x is
frees' (Pi1' x) is = frees' x is
frees' (Pi2' x) is = frees' x is
frees' (Pr' x z) is = frees' x is `S.union` frees' z is
frees' (App' x z) is = frees' x is `S.union` frees' z is
frees' (And' x z) is = frees' x is `S.union` frees' z is
frees' (Or' x z) is = frees' x is `S.union` frees' z is
frees' (Imp' x z) is = frees' x is `S.union` frees' z is

-- Just one case of eta-reduction
etaCompLam :: Lam' -> Lam'
etaCompLam t@(Abs' i t1) =
    case etaCompLam t1 of 
        t1'@(App' t2 (V' i')) -> if i==i' && not (i' `S.member` (frees0 t2)) then t2 else Abs' i t1'
        _ -> t
etaCompLam t = t

data Lam = C String | V Int 
         | Abs (Lam -> Lam) | App Lam Lam 
         | Pr Lam Lam | Pi Lam | Pi' Lam
         | Ex Lam | All Lam 
         | And Lam Lam | Or Lam Lam | Imp Lam Lam | Not Lam

instance Eq Lam where
   C xs == C ys = xs == ys
   
instance Show Lam where
    show = showLam
    
showLam (C xs) = mShowString xs
showLam (V i) = "x" ++ show i
showLam (Abs f) = 
    "\\x" ++ show i ++ "." ++ showLam (f (V i))
        where 
            i = var (Abs f)
showLam (Pr x y) = "<" ++ showLam x ++ "," ++ showLam y ++ ">"
showLam (Pi t) = 
    "pi" ++ showLam t
showLam (Pi' t) = 
    "pi'" ++ showLam t
showLam (App x y) = showLam x ++ "(" ++ showLam y ++ ")"
showLam (Ex (Abs f)) = 
    "[Ex" ++ show i ++ showLam (f (V i)) ++ "]"
        where 
            i = var (Abs f)
showLam (All (Abs f)) = 
    "[Ax" ++ show i ++ showLam (f (V i)) ++ "]"
        where 
            i = var (Abs f)
showLam (And t t') = 
    "[" ++ showLam t ++ "&" ++ showLam t' ++ "]"
showLam (Or t t') = 
    "[" ++ showLam t ++ "v" ++ showLam t' ++ "]"
showLam (Imp t t') = 
    "[" ++ showLam t ++ "->" ++ showLam t' ++ "]"
showLam (Not t) = 
    "[-" ++ showLam t ++ "]"

freshIdx :: S.Set Int -> Int
freshIdx s = 
    if S.null s
    then 1
    else succ (S.findMax s)

var :: Lam -> Int
var x = freshIdx (vars x S.empty)

vars :: Lam -> S.Set Int -> S.Set Int
vars (V i) _ = S.singleton i
vars (C _) _ = S.empty
vars (Abs f) is = vars (f (V i)) (S.insert i is) where i = freshIdx is
vars (All x) is = vars x is
vars (Ex x) is = vars x is
vars (Not x) is = vars x is
vars (Pi x) is = vars x is
vars (Pi' x) is = vars x is
vars (Pr x z) is = vars x is `S.union` vars z is
vars (App x z) is = vars x is `S.union` vars z is
vars (And x z) is = vars x is `S.union` vars z is
vars (Or x z) is = vars x is `S.union` vars z is
vars (Imp x z) is = vars x is `S.union` vars z is

infixr 9 <.>
(<.>) :: Lam -> Lam -> Lam
(Abs f) <.> (Abs g) = Abs (f.g)

infixr 9 $:
($:) :: Lam -> Lam -> Lam
(Abs f) $: g = f g
f $: g = App f g

pi1,pi2 :: Lam -> Lam
pi1 (Pr a _) = a
pi1 x = Pi x
pi2 (Pr _ a) = a
pi2 x = Pi' x

infix 8  ><,`And`,`Or`,`Imp`

(><) :: Lam -> Lam -> Lam
(><) = Pr

llf x = Abs (\ y -> y $: x)
low x = Abs (\ y -> Abs (\ z -> x $: Abs (\ w -> w $: z)))
mabs f = Abs (\ x -> f $: x)
--------------------------------------------
-- Combinator to Lambda term
--------------------------------------------
cb2lm :: CB -> Lam
cb2lm (I a) = Abs (\ x -> x)
cb2lm (KB 'l' _ _) = Abs (\ x -> Abs (\ y -> y $: x))
cb2lm (KB 'c' R _) = Abs (\ x -> Abs (\ y -> x >< y))
cb2lm (KB 'c' L _) = Abs (\ x -> Abs (\ y -> y >< x))
cb2lm (KB 'a' R _) = Abs (\ x -> pi1 x $: pi2 x)
cb2lm (KB 'a' L _) = Abs (\ x -> pi2 x $: pi1 x)
cb2lm (CB 'l' _) = Abs (\ x -> Abs (\ y -> y $: x))
cb2lm (CB 'c' R) = Abs (\ x -> Abs (\ y -> x >< y))
cb2lm (CB 'c' L) = Abs (\ x -> Abs (\ y -> y >< x))
cb2lm (CB 'a' R) = Abs (\ x -> pi1 x $: pi2 x)
cb2lm (CB 'a' L) = Abs (\ x -> pi2 x $: pi1 x)
cb2lm (Cp (f,g)) = 
    cb2lm f <.> cb2lm g
cb2lm (M c (f,g)) | c `elem` [P,X] =
    Abs (\ x -> (cb2lm f $: pi1 x) >< (cb2lm g $: pi2 x))
                  | otherwise = 
    Abs (\ x -> Abs (\ y -> cb2lm f $: (x $: cb2lm g $: y)))
cb2lm (Ap L (f,g)) = 
    Abs (\ x -> (cb2lm f $: pi2 x) $: (cb2lm g $: pi1 x))
cb2lm (Ap R (f,g)) = 
    Abs (\ x -> (cb2lm f $: pi1 x) $: (cb2lm g $: pi2 x))
--------------------------------------------
-- Labeled formulas
--------------------------------------------
type LF = (CB,F)
--------------------------------------------
-- Arrows
--------------------------------------------
type A = (CB,F,F)

type LA = (Lam,F,F)
--------------------------------------------
-- Labeled proof
--------------------------------------------
data HP = HP (Tree LA,Lam,String)

instance Show HP where
  show _ = ""

data HP1 = HP1 (Tree LA,Lam,String,String)

instance Show HP1 where
  show _ = ""

f3 :: (a -> b) -> (a,c,d) -> (b,c,d)
f3 f (a,b,c) = (f a,b,c)
--------------------------------------------
-- Proofs
--------------------------------------------
data Tree a = Tip | Lf a | Un a (Tree a) | Br a (Tree a) (Tree a)
    deriving (Eq,Ord,Show)

root (Lf a) = a
root (Un a _) = a
root (Br a _ _) = a

mapTr :: (a -> b) -> Tree a -> Tree b
mapTr f (Lf a) = Lf (f a)
mapTr f (Un r a) = Un (f r) (mapTr f a)
mapTr f (Br r a b) = Br (f r) (mapTr f a) (mapTr f b)

addNode x Tip = Lf x
addNode x (Lf a) = Un a (Lf x)
addNode x (Un a up) = Un a (addNode x up) 

type Proof = Tree A
--------------------------------------------
-- Trees with rule name
--------------------------------------------
data Tree' a = Lf' a | Un' String a (Tree' a) | Br' String a (Tree' a) (Tree' a)
    deriving (Eq,Ord,Show)

root' (Lf' a) = a
root' (Un' _ a _) = a
root' (Br' _ a _ _) = a

rule (Lf' a) = []
rule (Un' a _ _) = a
rule (Br' a _ _ _) = a

instance (Latex a) => Latex (Tree' a) where
  latex x = latexS x ""
  latexS (Lf' a) = latexS a
  latexS (Un' xs a b) = ("\\infer[" ++) . (latexRule xs ++) . ("]{" ++) . 
                        latexS a . ("}{" ++) . latexS b . ("}" ++)
  latexS (Br' xs a l r) = ("\\infer[" ++) . (latexRule xs ++) . ("]{" ++) . 
                          latexS a . ("}{\n" ++) .
                          latexS l . ("\n\t &\n\t " ++) . 
                          latexS r . ("}" ++)

latexRule :: String -> String
latexRule "ShR" = "\\triangleright"
latexRule "ShL" = "\\triangleleft"
latexRule "ScR" = "\\epsilon\\triangleright"
latexRule "ScL" = "\\triangleleft\\epsilon"
latexRule "CpR" = "\\circ\\triangleright"
latexRule "CpL" = "\\triangleleft\\circ"
--------------------------------------------
-- Latex
--------------------------------------------
class (Show a) => Latex a where
  latex :: a -> String
  latexS :: a -> ShowS

instance Latex a => Latex [a] where
  latex xs = latexS xs ""  
  latexS [] = ("" ++)
  latexS [a] = latexS a
  latexS (x:xs) = latexS x . (',':) . latexS xs

--latexS :: Latex a => [a] -> ShowS
--latexS [] = ("" ++)
--latexS (x:xs) = 
--      ("\\begin{tabular}{c}\n" ++) .
--           ('$':) . (latex x ++) . 
--           ('$':) . ("\\\\[.1cm]\n" ++) .
--           latexLs xs .
--      ("\\end{tabular}" ++)
--
--latexLs :: Latex a => [a] -> ShowS
--latexLs [] = ("" ++)
--latexLs (x:xs) = ("$" ++) . (latex x ++) . ("$" ++) . ("\\\\[.1cm]\n" ++) . latexLs xs 

instance Latex Bool where
  latex x = latexS x ""
  latexS True  = ("\\circ " ++)
  latexS False = ("\\bullet " ++)

instance Latex Char where
  latex a = latexS a ""
  latexS = mShowChar

mShowChar :: Char -> String -> String
mShowChar a 
    | a == '\228' = ("\\" ++) . (['"','a'] ++)
    | a == '\246' = ("\\" ++) . (['"','o'] ++)
    | a == '\196' = ("\\" ++) . (['"','A'] ++)
    | a == '\214' = ("\\" ++) . (['"','O'] ++)
    | otherwise = showLitChar a

mShowString :: String -> String
mShowString = foldr mShowChar ""

instance Latex Int where
  latex a = latexS a ""
  latexS a = shows a
  
instance (Latex a,Latex b) => Latex (a,b) where
  latex x = latexS x ""
  latexS (a,b) = ("{" ++) . latexS a . (" \\to " ++) . latexS b . ("}" ++)
  
instance (Latex a,Latex b,Latex c) => Latex (a,b,c) where
 latex x = latexS x ""
 latexS (a,b,c) = latexAr (a,b,c)
 
latexAr (a,b,c) = (latex a ++) . ("\\!:" ++) . (latex b ++) . ("\\to " ++) . (latex c ++)
latexSeq (a,b,c) = (latex b ++) . ("\\to " ++) . (latex c ++)
latexTriple (a,b,c) = ("(" ++) . (latex a ++) . (", " ++) . (latex b ++) . (", " ++) . (latex c ++) . (")" ++)

instance Latex C where
  latex x = latexS x ""
  latexS (X) = (", " ++)
  latexS (P) = (" \\otimes " ++) -- (" \\ast " ++)
  latexS (R) = ("/" ++)
  latexS (L) = ("\\bs " ++)

-- avoids the most external brackets on formulas
instance Latex F where
 latex x = latexS x "" 
 latexS S = ('S':)
 latexS (A xs) = (mShowString xs ++)
 latexS (B X a b) = latexFrm a . (", " ++) . latexFrm b
 latexS (B P a b) = latexFrm a . ("\\otimes " ++) . latexFrm b -- latexFrm a . ("\\ast " ++) . latexFrm b
 latexS x = latexFrm x

latexFrm :: F -> ShowS
latexFrm S = ('S':)
latexFrm (A xs) = (mShowString xs ++)
latexFrm (B c a b) = if c `elem` [R,L]
                     then latexFrm' a . latexS c . latexFrm' b
                     else ("(" ++) . latexFrm a . latexS c . latexFrm b . (")" ++)

latexFrm' :: F -> ShowS
latexFrm' S = ('S':)
latexFrm' (A xs) = (mShowString xs ++)
latexFrm' (B c a b) = if c `elem` [R,L]
                      then ("(" ++) . latexFrm' a . latexS c . latexFrm' b . (")" ++)
                      else ("(" ++) . latexFrm a . latexS c . latexFrm b . (")" ++)

instance (Latex a) => Latex (Tree a) where
  latex x = latexS x ""
  latexS Tip = ("" ++)
  latexS (Lf a) = latexS a
  latexS (Un a b) = ("\\infer{" ++) . 
                     latexS a . ("}{" ++) . latexS b . ("}" ++)
  latexS (Br a l r) = ("\\infer{" ++) . latexS a . ("}{\n" ++) .
                       latexS l . ("\n\t &\n\t " ++) . latexS r . ("}" ++)
                       
instance Latex HP where
  latex x = latexS x ""
  latexS (HP (p,l,xs)) = 
      ("\\begin{tabular}{c}\n" ++) .
      ("$" ++) . latexS p . ("$\\\\[.5cm]\n" ++) . 
      ("$" ++) . latexS l . ("$\\\\[.5cm]\n" ++) . 
                 (foldr latexS "" xs ++) .
      ("\\end{tabular}" ++)

instance Latex HP1 where
  latex x = latexS x ""
  latexS (HP1 (p,l,xs,ys)) = 
      ("\\begin{tabular}{c}\n" ++) .
      ("$" ++) . latexS p . ("$\\\\[.5cm]\n" ++) . 
      ("$" ++) . latexS l . ("$\\\\[.5cm]\n" ++) . 
                 (foldr latexS "" xs ++) . ("\\\\[.5cm]\n" ++) . 
                 (foldr latexS "" ys ++) .
      ("\\end{tabular}" ++)

instance Latex Lam where
    latex x = latexS x ""
    latexS (C xs) = ("\\text{\\sc " ++) . (mShowString xs ++) . ("}" ++)
    latexS (V i) = ("x_{" ++) . shows i . (++) "}"
    latexS (Abs f) = 
        ("\\lambda x_{" ++) . shows i . ("}." ++) . latexS (f (V i))
            where i = var (Abs f)
    latexS (Pr x y) = ("\\langle " ++) . latexS x . ("," ++) . latexS y . ("\\rangle" ++)
    latexS (Pi t) = 
        ("\\pi " ++) . latexS t
    latexS (Pi' t) = 
        ("\\pi' " ++) . latexS t
    latexS (App x y) = ("(" ++) . latexS x . ("\\ " ++) . latexS y . (")" ++)
    latexS (Ex (Abs f)) = 
        ("\\exists\\, x_{" ++) . shows i . ("}\\bo " ++) . latexS (f (V i)) . ("\\bb" ++)
            where i = var (Abs f)
    latexS (All (Abs f)) = 
        ("\\forall\\, x_{" ++) . shows i . ("}\\bo " ++) . latexS (f (V i)) . ("\\bb" ++)
            where i = var (Abs f)
    latexS (And t t') = 
        latexS t . ("\\wedge " ++) . latexS t'
    latexS (Or t t') = 
        latexS t . ("\\vee " ++) . latexS t'
    latexS (Imp t t') = 
        latexS t . ("\\to " ++) . latexS t'
    latexS (Not t) = 
        ("\\neg " ++) . latexS t

instance Latex Lam' where
    latex x = latexS x ""
    latexS (C' xs) = ("\\text{\\sc " ++) . (mShowString xs ++) . ("}" ++)
    latexS (V' i) = ("x_{" ++) . shows i . (++) "}"
    latexS (Abs' i f) = 
        ("\\lambda x_{" ++) . shows i . ("}." ++) . latexS f
    latexS (Pr' x y) = ("\\langle " ++) . latexS x . ("," ++) . latexS y . ("\\rangle" ++)
    latexS (Pi1' t) = 
        ("\\pi " ++) . latexS t
    latexS (Pi2' t) = 
        ("\\pi' " ++) . latexS t
    latexS (App' x y) = ("(" ++) . latexS x . ("\\ " ++) . latexS y . (")" ++)
    latexS (Ex' (Abs' i f)) = 
        ("\\exists\\, x_{" ++) . shows i . ("}\\bo " ++) . latexS f . ("\\bb" ++)
    latexS (All' (Abs' i f)) = 
        ("\\forall\\, x_{" ++) . shows i . ("}\\bo " ++) . latexS f . ("\\bb" ++)
    latexS (And' t t') = 
        latexS t . ("\\wedge " ++) . latexS t'
    latexS (Or' t t') = 
        latexS t . ("\\vee " ++) . latexS t'
    latexS (Imp' t t') = 
        latexS t . ("\\to " ++) . latexS t'
    latexS (Not' t) = 
        ("\\neg " ++) . latexS t

instance Latex CB where
  latex x = latexS x ""
  latexS (I a) = ("1_{" ++) . latexS a . ("}" ++)
  latexS (M P (f,g)) = ("(" ++) . latexS f . ("\\ast " ++) . latexS g . (")" ++)
  latexS (M X (f,g)) = {- ("(" ++) . -} latexS f . ("\\times " ++) . latexS g {- . (")" ++) -}
  latexS (M L (a,b)) = ("\\bbslash_{( " ++) . latexS a . ("," ++) . latexS b . (" )}" ++)
  latexS (M R (a,b)) = ("\\sslash_{( " ++) . latexS a . ("," ++) . latexS b .  (" )}" ++)
  latexS (CB 'l' c) = ("\\mathscr{L}^{" ++) . latexS c . ("}" ++)
  latexS (CB 'c' c) = ("\\mathscr{C}^{" ++) . latexS c . ("}" ++)
  latexS (CB 'a' c) = ("\\mathscr{A}^{" ++) . latexS c . ("}" ++)
  latexS (CB a c) = ("{" ++) . latexS a . ("}^{" ++) . latexS c . ("}" ++)
  latexS (KB 'l' c _) = ("\\mathscr{L}^{" ++) . latexS c . ("}" ++)
  latexS (KB 'c' c _) = ("\\mathscr{C}^{" ++) . latexS c . ("}" ++)
  latexS (KB 'a' c _) = ("\\mathscr{A}^{" ++) . latexS c . ("}" ++)
  latexS (Cp (f,g)) = latexS f . (" \\, \\cdot \\," ++) . latexS g
  latexS (Ap c (f,g)) = latexS f . ("^{" ++) .latexS c . ("}(" ++) . latexS g . (")" ++)

--------------------------------------------------------
-- Latex output
--------------------------------------------------------
wr :: Latex a => [a] -> IO ()
wr xs = do
        wrap (length xs)
        foldr (>>) (return ()) (writeg xs 1)

writeg :: (Enum a, Show a, Latex b) => [b] -> a -> [IO ()]
writeg [] _ = []
writeg (x:xs) i =
    writeg' x' i:writeg xs (succ i)
     where
      x' = (("\\ensuremath{" ++) . latexS x . ("}" ++)) ""

writeg' :: Show a => [Char] -> a -> IO ()
writeg' t i = writeFile ((("Tex/Eg/eg" ++) . shows i . (".tex" ++)) "") t

wrap :: Int -> IO ()
wrap stat = writeFile "Tex/proofs.tex"
               ((("\\documentclass[10pt]{article}\n" ++) .
                ("\\usepackage{amsmath,mathrsfs}\n" ++) .
                ("\\usepackage{graphicx}\n" ++) .
                ("\\usepackage{stmaryrd}\n" ++) .
                ("\\usepackage{proof}\n" ++) .
                ("\\usepackage{textcomp}\n" ++) .
                ("\\usepackage{ifthen}\n" ++) .
                ("\\usepackage{color}\n" ++) .
                ("\\usepackage{calc}\n" ++) .
                ("\\usepackage{latexsym}\n\n" ++) .
                ("\\RequirePackage[pdftex,pagebackref,pdfpagemode=none,colorlinks,%\n" ++) .
                ("             pdfmenubar=false,%\n" ++) .
                ("             pdftoolbar=false,%\n" ++) .
                ("             pdffitwindow=true,pdfcenterwindow=true,%\n" ++) .
                ("             pdfwindowui=false,menucolor=menucolor,%\n" ++) .
                ("             pdfview=Fit,pdfstartview=Fit]{hyperref}\n\n" ++) .
                ("\\pagestyle{empty}\n" ++) .
                ("\\definecolor{azzurro}{rgb}{0.1,0.2,0.8}\n" ++) .
                ("\\newcommand{\\textazz}[1]{\\mbox{\\scriptsize{\\textcolor{azzurro}{$#1$}}}}\n" ++) .
                ("\\newcommand{\\Boxd}{\\Box}\n" ++) .
                ("\\newcommand{\\bs}{\\backslash}\n" ++) .
                ("\\newcommand{\\bo}{[}\n" ++) .
                ("\\newcommand{\\bb}{]}\n" ++) .
                ("\\newcommand{\\dia}{\\diamondsuit}\n" ++) .
                ("\\newcommand{\\lx}{\\vartriangleleft}\n" ++) .
                ("\\newcommand{\\ev}{\\text{ev}}\n" ++) .
                ("\\newcommand{\\rx}{\\vartriangleright}\n\n\n" ++) .
                ("\\newlength{\\wdtotal}\n" ++) .
                ("\\newlength{\\httotal}\n" ++) .
                ("\\setlength{\\wdtotal}{0.0cm}\n" ++) .
                ("\\setlength{\\httotal}{0.0cm}\n\n" ++) .
                ((saveboxes 1 stat)  ++) .
                ("\\setlength{\\paperwidth}{2in+\\wdtotal}\n" ++) .
                ("\\setlength{\\paperheight}{3in+\\httotal}\n" ++) .
                ("\\textwidth = \\wdtotal\n" ++) .
                ("\\textheight = \\httotal\n\n" ++) .
                ("\\oddsidemargin = 0.0 in\n" ++) .
                ("\\evensidemargin = 0.0 in\n" ++) .
                ("\\topmargin = 0.0 in\n" ++) .
                ("\\headheight = 0.0 in\n" ++) .
                ("\\headsep = 0.0 in\n" ++) .
                ("\\parskip = 0.2in\n" ++) .
                ("\\parindent = 0.0in\n\n" ++) .
                ("\\begin{document}\n" ++) .
                ((useboxes 1 stat) ++) .
                ("\\end{document}\n\n" ++)) "")


saveboxes :: Int -> Int -> [Char]
saveboxes i n = if i <= n then
                let name = (proofboxname i)
                in
                (("%%\n" ++) .
                ("% box for eg" ++) .
                (show i ++) . (".tex" ++) .
                ("\n%%\n" ++) .
                ("\\newsavebox{\\" ++) . (name  ++) . ("}\n" ++) .
                ("\\sbox{\\" ++) . (name ++) . ("}{\\input{Eg/eg" ++) . (show i ++) . (".tex}}\n" ++) .
                ("\\ifthenelse{\\wd\\" ++) . (name ++) . (" > \\wdtotal}{\\setlength{\\wdtotal}{\\wd\\" ++) . (name ++) . ("}}{}\n" ++) .
                ("\\ifthenelse{\\ht\\" ++) . (name ++) . (" > \\httotal}{\\setlength{\\httotal}{\\ht\\" ++) . (name ++) . ("}}{}\n\n" ++) .
                ((saveboxes (succ i) n) ++)) ""
                else []

proofboxname :: Int -> [Char]
proofboxname i = "proofbox" ++ (lettercodes i)

lettercodes :: Int -> [Char]
lettercodes i =    if i <= 26
                   then
                     let r = i+96
                     in [chr r]
                   else
                     let s = i `mod` 26
                         m' = (i `div` 26)
                     in if s /= 0
                        then
                          let r' = s + 96
                          in (chr r'):(lettercodes m')
                        else
                          'z':(lettercodes m')

useboxes :: Int -> Int -> [Char]
useboxes i n = if i <= n then
               let name = (proofboxname i)
               in "\\usebox{\\" ++ name ++ "}\n\\newpage\n" ++
                  (useboxes (succ i) n)
               else []
