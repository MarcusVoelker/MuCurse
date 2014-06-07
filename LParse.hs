module LParse where

import Data.List

import Continuations

data Parser r a = Parser {pFunc :: String -> DCont r String (a,String)}

instance Functor (Parser r) where
    fmap f p = Parser (\s -> fmap (\(a,b) -> (f a,b)) (pFunc p s))

parse :: Parser r a -> String -> (a -> r) -> (String -> r) -> r
parse p s btr etr = run (pFunc p s) (btr . fst) etr

infixl 1 <.
infixr 0 .>

infixl 1 <*
infixr 0 *>

constParse :: a -> Parser r a
constParse a = Parser (\s -> return (a,s))

dot :: Parser r a -> (a -> b -> c) -> Parser r b  -> Parser r c
dot pa f pb = Parser (\s -> (pFunc pa s) >>= (\(a,r) -> fmap (\(b,r') -> (f a b,r')) (pFunc pb r)))

(<.) = (,)
(.>) = uncurry dot

weakDot :: Parser r a -> (a -> b -> a) -> Parser r b -> Parser r a
weakDot pa f pb = Parser (\s -> weakChain (pFunc pa s) (\(a,r) -> fmap (\(b,r') -> (f a b,r')) (pFunc pb r)))

(<*) = (,)
(*>) = uncurry weakDot

(<|>) :: Parser r a -> Parser r a -> Parser r a
(<|>) p1 p2 = Parser (\s -> branch (pFunc p1 s) (pFunc p2 s))

pEmpty :: Parser r [a]
pEmpty = Parser (\s -> DCont (\btr _ -> btr ([],s)))

plus :: Parser r a -> Parser r [a]
plus p1 = Parser (\s -> exceptChain (pFunc p1 s) (\(a,r) -> fmap (\(b,r') -> (a:b,r')) (pFunc (plus p1) r)) (\(a,s) -> ([a],s)))

star :: Parser r a -> Parser r [a]
star p = plus p <|> pEmpty

cParse :: (String -> Bool) -> Parser r a -> String -> Parser r a
cParse c p err = Parser (\s -> if c s then pFunc p s else throw err)

prefixParse :: String -> Parser r a -> Parser r a
prefixParse pre p = Parser (\s -> if isPrefixOf pre s then pFunc p s else throw ("Expected " ++ pre))

dPrefixParse :: String -> Parser r a -> Parser r a
dPrefixParse pre p = prefixParse pre (pParse (drop (length pre)) p)

pParse :: (String -> String) -> Parser r a -> Parser r a
pParse f p = Parser (\s -> pFunc p (f s))

pFail :: String -> Parser r a
pFail err = Parser (const $ throw err)

noopParse :: Parser r ()
noopParse = Parser (\s -> DCont (\btr _ -> btr ((),s)))

charParse :: (Char -> a) -> Parser r a
charParse f = Parser (\s -> DCont (\btr _ -> btr (f $ head s,tail s)))
