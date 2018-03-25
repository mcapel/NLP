module Main where

import Data
import IO
import ER
import Parse
import List
import System
import Char
import Lexer
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------    
main :: IO ExitCode
main = 
    do
     hSetBuffering stdin NoBuffering
     putStrLn "Welcome to NLP!"
     putStrLn "Press h for help."
     c <- getChar
     putStrLn ""
     case c of
       'q' -> exitWith ExitSuccess
       'h' -> help
       'f' -> fragment
       'a' -> nlpParseFrag
       _ -> again
----------------------------------------
----------------------------------------
----------------------------------------
----------------------------------------
again :: IO ExitCode
again = 
    do
     putStrLn "Action?"
     c <- getChar
     case c of
       'q' -> exitWith ExitSuccess
       'h' -> help
       'f' -> fragment
       'a' -> nlpParseFrag
       _ -> again

tex :: String -> IO ExitCode
tex xs = 
    system ("killall Preview; cd Tex; pdflatex " ++
    xs ++ ".tex; pdflatex " ++ 
    xs ++ ".tex; open " ++ 
    xs ++ ".pdf")

help :: IO ExitCode
help = 
    do
     putStrLn "q\t quit"
     putStrLn "h\t help"
     putStrLn "f\t fragment"
     putStrLn "a\t parse all fragment"
     putStrLn "t\t translates fragments"
     again

nlpParseFrag :: IO ExitCode
nlpParseFrag = 
    do 
     putStrLn "\nFragment:"
     str <- getLine 
     frag <- readFile ("Lex/" ++ str ++ ".nlp")
     (wr $ nlpParseAll frag) >> tex "proofs"
     again

nlpParseAll :: String -> [HP]
nlpParseAll frag =
    allParses tests lex
      where 
      ls = lines frag
      lex = parseLex ls
      tests = parseTests ls
    

wrParses' :: String -> String -> IO ()
wrParses' lex str = 
     wr $ mainParse s str lex'
      where 
      ls = lines lex
      lex' = parseLex ls

wrParses :: String -> String -> String -> IO ()
wrParses c lex str = 
     wr $ mainParse c' str lex'
      where 
      ls = lines lex
      lex' = parseLex ls
      c' = parseFrm c

parseFrm :: String -> F
parseFrm c = head (success formula c)

fragment :: IO ExitCode
fragment = 
    do 
     putStrLn "\nFragment:"
     lex <- getLine
     lex' <- readFile ("Lex/" ++ lex ++ ".nlp")
     writeFragment' lex'
     again

writeFragment' :: String -> IO ExitCode
writeFragment' lex = 
    (writeLex $ wrapLex lex' tests "") >> tex "lex"
      where 
      ls = lines lex
      lex' = parseLex ls
      tests = parseTests ls

parseLex :: [String] -> Lex
parseLex lex = 
    concatMap (success entry) lex

parseTests :: [String] -> [(String,F)]
parseTests lex = 
    concatMap (success testExp) lex

allParses :: [(String,F)] -> Lex -> [HP]
allParses tests lex = 
     [ p | (str,c) <- tests, p <- mainParse c str lex ]
