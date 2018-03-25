module Transl where

import Data

transl :: Lam -> Lex -> [(String,F)]
transl t@(C _) lex = 
    [ (w,a) | (w,a,t') <- lex, t==t' ]
transl (App t1 t2) lex =
    [ (w1 ++ " " ++ w2,a) | (w1,B R a b) <- transl t1 lex,
                            (w2,b') <- transl t2 lex,
                            b==b' ]
                       ++
    [ (w1 ++ " " ++ w2,a) | (w2,B L b a) <- transl t1 lex,
                            (w1,b') <- transl t2 lex,
                            b==b' ]
transl (Pr t1 t2) lex =
    [ (w1 ++ " " ++ w2,B P a b) | (w1,a) <- transl t1 lex,
                                  (w2,b) <- transl t2 lex ]
transl _ _ = []