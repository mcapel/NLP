module Lex where
import Data
import ER
-- Macros
q = A "q"
whq = A "whq"
bf = A "bf"
inf = A "inf"
prt = A "Prt"
p = A "P"
iv   = n *> s
tv   = iv <* n
av   = iv <* a
sv   = iv <* s
dv   = iv <* (n|*|n)
hv   = qs *> s
vp   = iv <* prt
p1   = p <* c
cp   = s <* s
adv  = iv <* iv
qs   = s <* iv
qo   = tv *> qs *> s
qo'  = (qs|*|tv) *> s
dts  = qs <* c
dto  = qo <* c
dto' = qo' <* c
cl   = (qo  *> (n|*|s)) <* iv
cl'  = (qo' *> (n|*|s)) <* iv
----------------------
-- Closure operations
----------------------
co,ap,lf :: Int -> F -> F -> F
co 0 a b = a
co (i+1) a b = (co i a b |*| b) <* b

ap 0 a b = a
ap (i+1) a b = (ap i a b <* b) |*| b

lf 0 a b = a
lf (i+1) a b = b <* (lf i a b *> b) 

co',ap',lf' :: Int -> F
co' i = co i a b
lf' i = lf i a b
ap' i = ap i a b
----------------------------------
-- Labeled Lexicon for Propositionl Logic
-- and evaluation of formulas
----------------------------------

--slexPL :: Lex
--slexPL = 
--        [ ("p",s,C "p"),
--          ("q",s,C "q"),
--          ("r",s,C "r"),
--          ("s",s,C "p"),
--          ("&",(s *> s) <* s,Abs (\ x -> (Abs (\ y -> y `And` x)))),
--          ("v",(s *> s) <* s,Abs (\ x -> (Abs (\ y -> y `Or` x)))),
--          ("-",s <* s,Abs (\ x -> Not x)),
--          ("->",s *> (s <* s),Abs (\ x -> (Abs (\ y -> x `Imp` y)))),
--          ("(",s<*(s|*|b),Abs (\ x -> pi1 x)),
--          (")",b,C "c")
--          ]
--
--eval a@(C _) = evalAt a
--eval (Not a) = not (eval a)
--eval (And a b) = eval a && eval b
--eval (Or a b) = eval a || eval b
--eval (Imp a b) = not (eval a) || eval b
--
--evalAt (C "p") = True
--evalAt (C "q") = True
--evalAt (C "r") = False
--evalAt (C "s") = False
--
-----------------------------
---- Labeled English Lexicon 
-----------------------------
--
--slexEng :: Lex
--slexEng = 
--        [ ("John",n,C "j"),
--          ("Mary",n,C "m"),
--          ("walks",iv,C "W"),
--          ("talks",iv,C "T"),
--          ("walk",bf,C "W"),
--          ("talk",bf,C "T"),
--          ("missing",c<*c,C "missing"),
--          ("happy",c<*c,C "happy"),
--          ("funny",c<*c,C "funny"),
--          ("kind",c<*c,C "kind"),
--          ("loves",tv,C "L"),
--          ("likes",tv,C "L"),
--          ("reads",tv,C "R"),
--          ("hates",tv,C "H"),
--          ("gives",iv<*(n|*|n),C "gives"),
--          ("meet",bf<*n,C "meet"),
--          ("to",inf<*bf,Abs id),
--          ("to",(n*>(n|*|n))<*n,Abs (\ x -> Abs (\ y -> y >< x))),
--          ("wants",iv<*inf,
--                   Abs (\ x -> Abs (\ y -> (C "want" $: x $: y) $: y))),
--          ("wants",iv<*(qs|*|inf),
--                   Abs (\ x -> Abs (\ y -> pi1 x $: Abs (\ z -> ((C "want" $: (pi2 x $: z)) $: y))))),
--          ("wants",iv<*(inf<*bf|*|(bf<*n|*|qs)),
--                   Abs (\ x -> Abs (\ y -> (pi2 (pi2 x) $: Abs (\ z -> ((C "want" $: (pi1 (pi2 x) $: z) $: y) $: y)))))),
--          ("be",bf<*(c<*c),C "be"),
--          ("is",hv<*(c<*c),
--                Abs (\ y -> Abs (\ x -> (C "be" $: y) $: x))),
--          ("is",tv,C "="),
--          ("seems",iv<*inf,
--                   Abs (\ x -> Abs (\ y -> (C "seem" $: (x $: y))))),
--          ("seems",iv<*(s<*s|*|s),
--                   Abs (\ x -> Abs (\ y -> (C "seem" $: (pi1 x $: pi2 x))))),
--          ("promised",iv<*inf,
--                      Abs (\ x -> Abs (\ y -> (C "promise" $: x $: y) $: y))),
--          ("promised",iv<*(qs|*|inf),
--                      Abs (\ x -> Abs (\ y -> pi1 x $: Abs (\ z -> ((C "promise" $: pi2 x $: y) $: z) $: y)))),
--          ("persuaded",iv<*(qs|*|inf),
--                       Abs (\ x -> Abs (\ y -> pi1 x $: Abs (\ z -> (((C "persuade" $: (pi2 x $: z)) $: z) $: y))))),
--          ("he",qs,llf (V 5)),
--          ("it",qs,llf (V 7)),
--          ("him",qo',Abs (\ x -> pi1 x $: pi2 x $: V 2)),
--          ("man",c,C "man"),
--          ("woman",c,C "woman"),
--          ("book",c,C "book"),
--          ("himself",tv*>n*>s,Abs (\ x -> Abs (\ y -> (x $: y) $: y))),
--          ("herself",tv*>n*>s,Abs (\ x -> Abs (\ y -> (x $: y) $: y))),
--          ("that",(c*>c)<*(qs|*|tv),
--                  Abs (\ x -> Abs (\ y -> Abs (\ z -> y $: z `And` pi1 x $: pi2 x $: z)))),
--          ("that",s<*s,Abs id),
--          ("who",(c*>c)<*(tv|*|qo'),
--                 Abs (\ x -> Abs (\ y -> Abs (\ z -> y $: z `And` pi2 x $: (llf z >< pi1 x))))),
--          ("who",whq<*iv,Abs (\ x -> Abs (\ y -> x $: y))),
--          ("who",whq<*(q<*(qs|*|bf)|*|(qs|*|bf<*n)),
--                 Abs (\ x -> Abs (\ y -> pi1 x $: (pi1 (pi2 x) >< pi2 (pi2 x) $: y)))),
--          ("did",q<*(qs|*|bf),
--                 Abs (\ x -> Abs (\ y -> pi1 x $: pi2 x `And` y))),
--          ("did",q<*(qs|*|(bf<*n|*|qs)),
--                 Abs (\ x -> Abs (\ y -> pi1 x $: Abs (\ z -> pi2 (pi2 x) $: Abs (\ w -> (pi1 (pi2 x) $: w) $: z) `And` y))))
--          ]
--          ++
--          ands
--          ++
--          ex
--          ++
--          un
--          ++
--          detEx
--          ++
--          detUn
--
--ands = 
--      [ ("and",s*>(s<*s),Abs (\ x -> Abs (\ y -> x `And` y))),
--        ("and",iv*>(iv<*iv),Abs (\ z -> Abs (\ x -> Abs (\ y -> z $: y `And` x $: y)))),
--        ("and",qs*>(qs<*qs),Abs (\ z -> Abs (\ x -> Abs (\ y -> z $: y `And` x $: y))))
--         ]
--ex = 
--     [ ("someone",qs,Abs (\ y -> Ex (Abs (\ x -> y $: x)))),
--       ("someone",qo,Abs (\ x -> Abs (\ y -> Ex (Abs (\ z -> y $: x $: z))))),
--       ("someone",qo',Abs (\ x -> Ex (Abs (\ z -> pi1 x $: pi2 x $: z)))) ]
--
--un = 
--     [ ("everyone",qs,Abs (\ y -> All (Abs (\ x -> y $: x)))),
--       ("everyone",qo,Abs (\ x -> Abs (\ y -> All (Abs (\ z -> y $: x $: z))))),
--       ("everyone",qo',Abs (\ x -> All (Abs (\ z -> pi1 x $: pi2 x $: z)))) ]
--
--detEx = [ -- ("a",n<*c,Abs (\ z -> Abs (\ y -> Ex (Abs (\ x -> y $: x))))),
--          ("a",qs<*c,Abs (\ z -> Abs (\ y -> Ex ((Abs (\ x -> z $: x `And` y $: x)))))),
--          ("a",qo<*c,Abs (\ w -> Abs (\ x -> Abs (\ y -> Ex (Abs (\ z -> w $: z `And` y $: x $: z)))))),
--          ("a",qo'<*c,Abs (\ w -> Abs (\ x -> Ex (Abs (\ z -> w $: z `And` pi1 x $: pi2 x $: z))))) ]
--
--detUn = [ ("every",qs<*c,Abs (\ z -> Abs (\ y -> All (Abs (\ x -> z $: x `And` y $: x))))),
--          ("every",qo<*c,Abs (\ w -> Abs (\ x -> Abs (\ y -> All (Abs (\ z -> w $: z `And` y $: x $: z)))))),
--          ("every",qo'<*c,Abs (\ w -> Abs (\ x -> All (Abs (\ z -> w $: z `And` pi1 x $: pi2 x $: z))))) ]
--
------------------------------------------
---- Examples
------------------------------------------
--s0 = "John loves Mary"                     -- s
--s1 = "everyone loves someone"              -- s
--s2 = "someone is missing"                  -- s
--s21 = "John is missing"                    -- qs*iv
--s3 = "John persuaded a woman to be kind"   -- s
--s4 = "John promised a woman to be kind"    -- s
--s41 = "John seems to walk"                 -- s
--s42 = "it seems that John walks"           -- s
--s5 = "did John meet a woman"               -- q
--s6 = "who did John meet"                   -- whq
--s7 = "John wants to meet a woman"          -- s
--s8 = "John wants to meet a woman who likes every book"    -- s
--s9 = "every man hates a book that every woman loves"    -- s
--s10 = "John gives Mary a book"    -- s
--s11 = "John gives a book to Mary"    -- s
--
--ss = [ 
--      s0
--     ,s1
--     ,s2
--     ,s21
--     ,s3
--     ,s4
--     ,s41
--     ,s42
--     ,s7
--     ,s8
--     ,s9 
--    ]
