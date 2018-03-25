module Mix(
    Item,
    It,
    Prem(PAx,PCp,PPr,PSh),
    Cell,
    Chart,
    Agenda,
    direction,
    axiom,
    isAxiom,
    table,
    recognize,
    updateChart,
    recognize,
    findGoals,
    exhaust,
    axioms,
    output,
    infer,
    inferences,
    complete,
    predict,
    shift
    ) where

import Debug.Trace
import Data
import Data.Array
import qualified Data.Set as S
import qualified Data.Map as M

data Item = It (String,[F],[F],F)
    deriving (Ord,Eq)

instance Show Item where
  show (It (xs,ys,zs,c)) =
    "(" ++ show xs 
    ++ "," ++ show ys 
    ++ "," ++ show zs 
    ++ " |- " ++ show c ++ ")"
    
instance Latex Item where
  latex x = latexS x ""
  latexS (It ("R",ys,zs,c)) =
    latexS ys . ("\\triangleright " ++) . 
    latexS zs . ("\\to " ++) . latexS c
  latexS (It ("L",ys,zs,c)) =
    latexS (reverse zs) . ("\\triangleleft " ++) . 
    latexS ys . ("\\to " ++) . latexS c
  latexS (It ([],ys,[],c)) =
    latexS ys . ("\\to " ++) . latexS c

direction :: Item -> C
direction (It ([c],_,_,_)) 
    | c == 'L' = L
    | c == 'R' = R

axiom :: F -> Item
axiom a = It ([],[a],[],a)

isAxiom :: Item -> Bool
isAxiom (It ([],[a],[],b)) = True
isAxiom _ = False

type It = ((Int,Int),Item)

direction' :: It -> C
direction' = direction.snd

data Prem =  PSh It -- link to the premise of a shifting
           | PAx (Lam',CB) -- link to the lexical recipes for an axiom
           | PPr -- no link to the premise of a product prediction: they are axioms
           | PCp (It,It) -- link to the premises of a completion
    deriving (Eq,Ord,Show)

type Cell = M.Map Item (S.Set Prem)
type Chart = Array (Int,Int) Cell

table :: Int -> Chart
table n = array ((0,0),(n,n)) [ ((i,j),M.empty) | 
                                    i <- [0..n],
                                    j <- [0..n] ] 

updateChart :: (It,Prem) -> Chart -> Chart
updateChart ((k,it),prem) chart = 
    accum (M.unionWith (S.union)) chart [cell]
        where
          cell = (k,M.singleton it (S.singleton prem))

updatesChart :: [(It,Prem)] -> Chart -> Chart
updatesChart [] c = c
updatesChart (i:is) chart = updatesChart is (updateChart i chart)

type Agenda = [(It,Prem)]

axioms' :: Lex' -> [String] -> Agenda
axioms' lex ws = 
    axioms 0 lex ws

axioms :: Int -> Lex' -> [String] -> Agenda
axioms _ lex [] = []
axioms i lex (w:ws) = 
    word (i,j) w lex ++ axioms j lex ws
     where j = i+1 

word :: (Int,Int) -> String -> Lex' -> Agenda
word k w lex =
    [ ((k,axiom a),PAx (lam,cb)) | (lam,cb,w',_,a) <- lex, w==w' ]

recognize :: Int -> [(CB,F)] -> [String] -> Lex' -> (Chart,Agenda)
recognize n outs ws lex = 
    trace (show agenda ++ "\n\n") exhaust n (chart,outs' ++ agenda)
      where 
        n = length ws
        chart = table n
        agenda = axioms' lex ws
        outs' = output outs

findGoals :: [(CB,F)] -> Int -> Chart -> [(CB,Item)]
findGoals cs n chart = goals' cs (M.keys $ chart ! (0,n))

goals' :: [(CB,F)] -> [Item] -> [(CB,Item)]
goals' cs is = goals cs [] is

goals :: [(CB,F)] -> [Item] -> [Item] -> [(CB,Item)]
goals [] _ _ = []
goals (_:cs) is [] = goals' cs is
goals cs@((f,c):_) is' (it@(It (_,_,[],c')):is) 
    | c==c' = (f,it):goals cs is' is 
    | otherwise = goals cs (it:is') is
goals cs is' (_:is) = goals cs is' is

output :: [(CB,F)] -> [(It,Prem)]
output outs = 
    [ (it,prem) | (_,a) <- outs, 
      (it,prem) <- predict ((0,0),It ("R",[],[a],a)) ]

exhaust :: Int -> (Chart,Agenda) -> (Chart,Agenda)
exhaust n (chart,[]) = trace (show chart) (chart,[])
exhaust n (chart,y@((k,y'),prem):agenda) =
        let 
          c = chart ! k
        in if y' `M.member` c
           then exhaust n (updateChart y chart,agenda)
           else exhaust n (infer n y (chart,agenda))

infer :: Int -> (It,Prem) -> (Chart,Agenda) -> (Chart,Agenda)
infer n it@(it',prem) (chart,agenda) = 
    (chart',agenda')
    where
     cs = inferences n it' chart
     agenda' = agenda ++ cs
     chart' = updatesChart (it:cs) chart 

inferences :: Int -> It -> Chart -> [(It,Prem)]
inferences n it chart = 
    complete n it chart 
    ++ 
    predict it 
    ++ 
    shift it

complete :: Int -> It -> Chart -> [(It,Prem)]
complete n ((i,k),it2@(It ("R",xs,a:ys,c))) chart = 
      rightComplete (i,k,k) n it2 chart
       where
        rightComplete :: (Int,Int,Int) -> Int -> Item -> Chart -> [(It,Prem)]
        rightComplete (i,k,j) n it2@(It ("R",xs,a:ys,c)) chart 
            | j<=n = 
                [ (((i,j),It ("R",xs++[a],ys,c)),PCp (((i,k),it1),((k,j),it2))) | 
                    it1@(It (_,_,[],b)) <- M.keys $ chart ! (k,j), a==b ]
                ++
                rightComplete (i,k,(j+1)) n it2 chart
            | otherwise = []
complete _ ((k,j),it2@(It ("L",xs',a:ys,c))) chart = 
    leftComplete (k,k,j) it2 chart
     where 
      leftComplete :: (Int,Int,Int) -> Item -> Chart -> [(It,Prem)]
      leftComplete (i,k,j) it2@(It ("L",xs,a:ys,c)) chart
            | 0<=i =
                [ (((i,j),It ("L",a:xs,ys,c)),PCp (((i,k),it1),((k,j),it2))) | 
                    it1@(It (_,_,[],b)) <- M.keys $ chart ! (i,k), a==b ]
                ++
                leftComplete ((i-1),k,j) it2 chart
            | otherwise = []
--complete n ((i,j),it1@(It (_,_,[],a))) chart =
--    completeR (i,i,j) it1 chart ++ completeL (i,j,j) n it1 chart
--     where
--      completeR :: (Int,Int,Int) -> Item -> Chart -> [(It,Prem)]
--      completeR (i,k,j) it1@(It (_,_,[],a)) char
--        | 0<=i = [ (((i,j),It ("R",ys++[b],xs,c)),PCp (((k,j),it1),((i,k),it2))) | 
--                    it2@(It ("R",ys,b:xs,c)) <- M.keys $ chart ! (i,k), a==b ]
--                 ++
--                 completeR ((i-1),k,j) it1 char
--        | otherwise = []
--      completeL :: (Int,Int,Int) -> Int -> Item -> Chart -> [(It,Prem)]
--      completeL (i,k,j) n it1@(It (_,_,[],a)) char
--        | j<=n = [ (((i,j),It ("L",b:ys,xs,c)),PCp (((i,k),it1),((k,j),it2))) | 
--                    it2@(It ("L",ys,b:xs,c)) <- M.keys $ chart ! (k,j), a==b ]
--                 ++ 
--                 completeL (i,k,(j+1)) n it1 char
--        | otherwise = []
complete _ _ _ = []

predict :: It -> [(It,Prem)]
predict it@((i,j),It ("L",xs,y@(B P a b):ys,c)) =
    [ (((i,i),It ("L",[],[b,a],y)),PPr) ]
predict it@((i,j),It ("R",xs,y@(B P a b):ys,c)) =
    [ (((j,j),It ("R",[],[a,b],y)),PPr) ]
predict _ = []

shift :: It -> [(It,Prem)]
shift it@((i,j),It (_,xs,[],B R c b)) =
    [ (((i,j),It ("R",xs,[b],c)),PSh it) ]
shift it@((i,j),It (_,xs,[],B L b c)) =
    [ (((i,j),It ("L",xs,[b],c)),PSh it) ]
shift _ = []
     
