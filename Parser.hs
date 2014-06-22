module Parser (parseFunctionDefs, parseParams) where

import Control.Applicative((<$>))
import Data.Char
import Data.Maybe

import Function
import Continuations
import LParse

parseFunctionDefs :: [String] -> [FunctionDef]
parseFunctionDefs = mapMaybe tryParseFunctionDef

tryParseFunctionDef :: String -> Maybe FunctionDef
tryParseFunctionDef s = run (parseFunctionDef s) Just (const Nothing)

parseFunctionDef :: String -> DCont r String FunctionDef
parseFunctionDef ss = do
  (UDef s,r) <- parseUDef ss
  if head r == '=' then do
    f <- parseFunction (tail r)
    return (FunctionDef s f)
  else
    throw "Expected '='"

parseFunction :: String -> DCont r String Function
parseFunction ss = fst <$> pFunc parseFunctionStep ss

parseFunctionStep :: Parser r Function
parseFunctionStep = cParse (not . null) (parseS <|> parseC <|> parseP <|> parseA <|> parseR <|> parseM <|> parseU <|> pFail "Expected FunctionPrefix") "EmptyString"

parseS :: Parser r Function
parseS = dPrefixParse "S" (constParse S)

parseC :: Parser r Function
parseC = dPrefixParse "C" (constParse C)

parseP :: Parser r Function
parseP = dPrefixParse "P" (fmap P parseInt)

parseA :: Parser r Function
parseA = dPrefixParse "A" (parseFunctionStep <. Subs .> parseFunctions)

parseR :: Parser r Function
parseR = dPrefixParse "R" (parseFunctionStep <. PRek .> parseFunctionStep)

parseM :: Parser r Function
parseM = dPrefixParse "M" (fmap MRek parseFunctionStep)

parseU :: Parser r Function
parseU = dPrefixParse "U" (fmap UDef parseFName)

parseFunctions :: Parser r [Function]
parseFunctions = dPrefixParse "(" (plus parseFunctionStep <.const.> remCB)
    where remCB = cParse (not . null) (pParse tail noopParse) "Expected ')'"

parseInt :: Parser r Int
parseInt = fmap (foldl (\a b -> a*10 + b) 0) (plus parseDigit)

parseDigit :: Parser r Int
parseDigit = cParse (\s -> not (null s) && isDigit (head s)) (charParse digitToInt) "Expected Numeral"

parseUDef:: String -> DCont r String (Function,String)
parseUDef [] = return (UDef [],[])
parseUDef (s:ss) | ('a' <= s && s <= 'z') || ('0' <= s && s <= '9') = parseUDef ss >>= (\(UDef ss',r) -> return (UDef (s:ss'),r))
parseUDef ss = return (UDef [], ss)

parseFName :: Parser r String
parseFName = plus parseFChar

parseFChar :: Parser r Char
parseFChar = cParse (\s -> not (null s) && isDigit (head s) || isAsciiLower (head s)) (charParse id) "Expected lower case Digit"

parseParams :: String -> [Int]
parseParams [] = []
parseParams x = run (pFunc parseInt x) (\(i,r) -> if null r then [i] else i : parseParams (tail r)) (const [])
