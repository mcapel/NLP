module Parse(
    mainParse,
    parseAll,
    parse,
    parse',
    compose',
    compose
    ) where

import Data
import ER
import Mix
import Data.Array
import qualified Data.Set as S
import qualified Data.Map as M

mainParse :: F -> String -> Lex -> [HP]
mainParse c ws lex = 
     [ HP (lamTree t,v,ws) | (v,t) <- parseAll cs ws' lex' ]
      where
       cs = e c
       ws' = words ws
       lex' = expand lex
       lamTree :: CB -> Tree (Lam,F,F)
       lamTree t = mapTr (f3 cb2lm) t'
            where
              t' = cb2pr $ etaComp t

parseAll :: [(CB,F)] -> [String] -> Lex' -> [(Lam,CB)]
parseAll cs xs lex = 
   [ (cb2lm (Cp (f,t)) $: l,Cp (f,t)) | (f,g) <- gs, (l,t) <- parse ((0,n),g) chart ]
    where
      n = length xs
      (chart,_) = recognize n cs xs lex
      gs = findGoals cs n chart

parse :: It -> Chart -> [(Lam,CB)]
parse (k,it) chart =
    case M.lookup it (chart ! k) of
        Just ps -> parse' it (S.toList ps) chart
        Nothing -> []

parse' :: Item -> [Prem] -> Chart -> [(Lam,CB)]
parse' _ [] _ = []
parse' it (PAx (l,f):ps) chart =
    (reduce l,f):parse' it ps chart
parse' it (PSh it':_) chart = 
    [ (l,Cp (Sh c,f)) | (l,f) <- parse it' chart ] 
        where
         c = direction it
parse' it (PPr:_) chart = 
    [(C "Pr",CB 'p' c)]
        where
         c = direction it
parse' it (PCp (it1,it2):ps) chart =
    compose' funs args
    ++ 
    parse' it ps chart
        where
         args = parse it1 chart
         funs = parse it2 chart 

compose' :: [(Lam,CB)] -> [(Lam,CB)] -> [(Lam,CB)]
compose' funs args = 
    [ compose f a | f <- funs, a <- args ]

compose :: (Lam,CB) -> (Lam,CB) -> (Lam,CB)
compose (l1,Cp (Sh L,f)) (l2,g) = 
    (l2 >< l1,Ap L (f,g))
compose (l1,Cp (Sh R,f)) (l2,g) = 
    (l1 >< l2,Ap R (f,g))
compose (_,CB 'p' L) (l2,g) = 
    (l2,Cp (CB 'p' L,g))
compose (_,CB 'p' R) (l2,g) = 
    (l2,Cp (CB 'p' R,g))
compose (l1,Cp (CB 'p' L,f)) (l2,g) = 
    (l2 >< l1,M X (g,f))
compose (l1,Cp (CB 'p' R,f)) (l2,g) = 
    (l1 >< l2,M X (f,g))

