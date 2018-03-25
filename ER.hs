module ER(expand,(==>),e,r) where

import Data
import List
-- For all a,b in F, let AX0 = { a->a, a*a\b->b, a->(a*b)/b, a->b/(a\b) } plus their symmetric.
-- Define AX as the smallest set containing AX0 and closed under the following rules: 
-- 1)  if a->b in AX, then a*c->b*c, a/c->b/c, c/b->c/a and their symmetric are in AX.
-- 2a) if a->b in AX, |a|<|b|, and b->c in AX, |b|<|c|, then a->c in AX
-- 2b) if a->b in AX, |a|>|b|, and b->c in AX, |b|>|c|, then a->c in AX

infix 3 ==>
(==>) :: F -> F -> [CB]
a ==> c = [Cp (g,f) | (g,b') <- e c, (f,b) <- r a, b==b' ]
--------------------------
-- Lexical expansion
--------------------------
expand :: Lex -> Lex'
expand lex = 
            [ (t,etaComp f,w,a,c) |
                (w,a,t) <- lex,
                (f,c) <- r a ]
--------------------------
-- Expansion and Reduction
--------------------------
e,r :: F -> [LF]
-- Expansion:
-- e c = [ (f,a) | |a|<|c| or a==c and a->c in AX with proof f ]
e a@(A _) = [(I a,a)]
e (B P a b) = 
    [ (M P (f,g),B P a' b') | (f,a') <- e a, 
                              (g,b') <- e b ]
e (B R a b) = 
    mon
    ++
    [ (cop R f,c) | (f,B _ (B P c x) x') <- mon, x==x' ]
    ++
    [ (lif R f,c) | (f,B _ x' (B L c x)) <- mon, x==x' ]
     where
      mon = [ (M R (f,g),B R a' b') | (f,a') <- e a, 
                                      (g,b') <- r b ]
e (B L b a) = 
    mon
    ++
    [ (cop L f,c) | (f,B _ x' (B P x c)) <- mon, x==x' ]
    ++
    [ (lif L f,c) | (f,B _ (B R x c) x') <- mon, x==x' ]
     where
      mon = [ (M L (f,g),B L b' a') | (f,a') <- e a, 
                                      (g,b') <- r b ]

-- Reduction:
-- r a = [ (f,c) | |a|>|c| or a==c and a->c in AX with proof f ]
r a@(A _) = [(I a,a)]
r (B P a b) = 
   mon 
   ++ 
   [ (app R f,c) | (f,B _ (B R c x) x') <- mon, x==x' ]
   ++
   [ (app L f,c) | (f,B _ x' (B L x c)) <- mon, x==x' ]
    where 
      mon = [ (M P (f,g),B P a' b') | (f,a') <- r a, 
                                      (g,b') <- r b ]
r (B R a b) = 
    [ (M R (f,g),B R a' b') | (f,a') <- r a, 
                              (g,b') <- e b ]
r (B L b a) = 
    [ (M L (f,g),B L b' a') | (f,a') <- r a, 
                              (g,b') <- e b ]

