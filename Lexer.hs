module Lexer(
    Entry,
    Parser,
    parserCB,
    item,
    result,
    bind,
    zero,
    plus,
    success,
    sat,
    dot,
    char,
    digit,
    lower,
    upper,
    letter,
    alphanum,
    alphanum',
    word,
    word',
    word1,
    sc,
    thisWord,
    formula,
    brackFrm,
    ls,
    rs,
    pr,
    atom,
    abstract,
    application,
    pair,
    pr1,
    pr2,
    var,
    con,
    ex,
    all,
    not,
    and,
    or,
    imp,
    term,
    entry,
    testExp
    ) where

import Data hiding (var)
import Char
import Monad
import Prelude hiding (and,or,not,all)

newtype Parser a = Parser (String -> [(a,String)])
    
instance Monad Parser where
  return  = result
  (>>=) = bind

parserCB :: Parser a -> String -> [(a, String)]
parserCB (Parser p) = p

item :: Parser Char
item = Parser (\inp -> case inp of 
                      [] -> []
                      (x:xs) -> [(x,xs)])

result :: a -> Parser a
result v = Parser (\inp -> [(v,inp)])

bind :: Parser a -> (a -> Parser b) -> Parser b
p `bind` f = Parser 
                (\inp -> 
                    concat [ parserCB (f v) inp' | (v,inp') <- parserCB p inp ])

instance MonadPlus Parser where
   mzero = zero
   mplus = plus

zero :: Parser a
zero = Parser (\inp -> [])

plus :: Parser a -> Parser a -> Parser a
p `plus` q = Parser (\inp -> (parserCB p inp ++ parserCB q inp))

success :: Parser a -> String -> [a]
success p xs = [ a | (a,"") <- parserCB p xs ]

sat :: (Char -> Bool) -> Parser Char
sat p = item `bind` \x -> 
        if p x then result x else zero
        
dot :: Parser Char
dot = sat ('.'==)

char :: Char -> Parser Char
char x = sat (x==)

digit :: Parser Char
digit = sat (\x -> '0' <= x && x <= '9')

lower :: Parser Char
lower = sat (\x -> 'a' <= x && x <= 'z'
             || x=='\228' || x=='\246')

upper :: Parser Char
upper = sat (\x -> 'A' <= x && x <= 'Z'
             || x=='\196' || x=='\214')

letter :: Parser Char
letter = 
    lower `plus` upper

alphanum :: Parser Char
alphanum = letter `plus` digit

alphanum' :: Parser Char
alphanum' = letter `plus` digit `plus` char ' '

word :: Parser String
word = do { c <- alphanum; cs <- word; return (c:cs) } `plus` return ""

many :: Parser a -> Parser [a]
many p = do { x <- p; xs <- many p; return (x:xs) } `plus` return []

--word' :: Parser String
--word' = do { c <- alphanum'; cs <- word'; return (c:cs) } `plus` return ""
word' :: Parser String
word' = do { c <- alphanum'; 
             cs <- word'; 
             return (c:cs) } 
        `plus` 
        return ""

word1 :: Parser String
word1 = do { ws <- word';
             _ <- char '-';
             ws' <- word';
             return (ws ++ " " ++ ws')}
        `plus` 
        word'
        `plus` 
        return ""

sc :: Parser String
sc = do { c <- upper; cs <- sc; return (c:cs) } `plus` return ""

thisWord :: String -> Parser String
thisWord [] =  return ""
thisWord (x:xs) = 
    do { x' <- char x;
         xs' <- thisWord xs;
         result (x':xs') }
-----------------------
-- Formula parser 
-----------------------
formula :: Parser F
formula = 
    ls 
    `plus` 
    rs 
    `plus` 
    pr 
    `plus` 
    brackFrm 
    
brackFrm = 
          do { _ <- char '(';
               x <- formula;
               _ <- char ')';
               result x }
             `plus` 
              atom

ls = 
          do { x <- brackFrm;
               _ <- char '\\';
               y <- brackFrm;
               result (B L x y) }
rs = 
          do { x <- brackFrm;
               _ <- char '/';
               y <- brackFrm;
               result (B R x y) }
pr = 
          do { x <- brackFrm;
               _ <- char '*';
               y <- brackFrm;
               result (B P x y) }
atom = 
          do { x <- word;
               result (A x) }
-----------------------
-- Lambda parser 
-----------------------
abstract = 
    do { _ <- char '\\';
         c <- digit;
         _ <- char '.';
         t <- term;
         result (Abs' (digitToInt c) t) }
application = 
    do { _ <- char '(';
         t1 <- term;
         _ <- char ' ';
         t2 <- term;
         _ <- char ')';
         result (App' t1 t2) }
pair = 
    do { _ <- char '<';
         t1 <- term;
         _ <- char ',';
         t2 <- term;
         _ <- char '>';
         result (Pr' t1 t2) }
pr1 = 
    do { _ <- thisWord "p1(";
         t <- term;
         _ <- char ')';
         result (Pi1' t) }
pr2 = 
    do { _ <- thisWord "p2(";
         t <- term;
         _ <- char ')';
         result (Pi2' t) }
var = 
    do { i <- digit;
         result (V' (digitToInt i)) }
con = 
    do { _ <- char '*';
         w <- word;
         _ <- char '*'; 
         result (C' w) }
ex =
    do { _ <- thisWord "[Ex ";
         t <- abstract;
         _ <- char ']';
         result (Ex' t) }
all =
    do { _ <- thisWord "[All ";
         t <- abstract;
         _ <- char ']';
         result (All' t) }
not =
    do { _ <- thisWord "[Not ";
         t <- term;
         _ <- char ']';
         result (Not' t) }
and =
    do { _ <- char '[';
         t1 <- term;
         _ <- thisWord " & ";
         t2 <- term;
         _ <- char ']';
         result (And' t1 t2) }
or =
    do { _ <- char '[';
         t1 <- term;
         _ <- thisWord " v ";
         t2 <- term;
         _ <- char ']';
         result (Or' t1 t2) }
imp =
    do { _ <- char '[';
         t1 <- term;
         _ <- thisWord " => ";
         t2 <- term;
         _ <- char ']';
         result (Imp' t1 t2) }

term :: Parser Lam'
term = 
    abstract
    `plus`
    application
    `plus`
    pair
    `plus`
    pr1
    `plus`
    pr2
    `plus`
    var
    `plus`
    con
    `plus`
    ex
    `plus`
    all
    `plus`
    not
    `plus`
    and
    `plus`
    or
    `plus`
    imp
-----------------------
-- Lex parser 
-----------------------
type Entry = (String,F,Lam')

entry :: Parser Entry
entry = 
    do { ws <- word;
         _ <- thisWord " :: ";
         a <- formula;
         _ <- thisWord " :: ";
         t <- term;
         return (ws,a,etaCompLam t) }
-----------------------
-- Test Expressions
-----------------------
testExp :: Parser (String,F)
testExp = 
    do { _ <- char '%';
         _ <- char ' ';
         str <- word1;
         _ <- char ' ';
         _ <- thisWord "->";
         _ <- char ' ';
         c <- formula;
         return (str,c) }

