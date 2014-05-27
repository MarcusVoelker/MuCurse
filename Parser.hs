module Parser (parseFunctionDefs, parseParams) where

import Data.List
import Data.Char
import Data.Maybe

import Function
import Continuations

parseFunctionDefs :: [String] -> [FunctionDef]
parseFunctionDefs ss = catMaybes (map tryParseFunctionDef ss)

tryParseFunctionDef :: String -> Maybe FunctionDef
tryParseFunctionDef s = run (parseFunctionDef s) Just (const Nothing)

tryParseFunction :: String -> Maybe Function
tryParseFunction s = run (parseFunction s) Just (const Nothing)

parseFunctionDef :: String -> DCont r String FunctionDef
parseFunctionDef ss = do
  (UDef s,r) <- parseUDef ss
  if (head r == '=') then do
    f <- parseFunction (tail r)
    return (FunctionDef s f)
  else
    throw "Expected '='"

parseFunction :: String -> DCont r String Function
parseFunction ss = parseFunctionStep ss >>= (return . fst)

parseFunctionStep :: String -> DCont r String (Function,String)
parseFunctionStep [] = throw "Empty String"
parseFunctionStep ('S':ss) = return (S,ss)
parseFunctionStep ('C':ss) = return (C,ss)
parseFunctionStep s@('P':ss) = parseProj s
parseFunctionStep s@('A':ss) = parseSubs s
parseFunctionStep s@('R':ss) = parsePRek s
parseFunctionStep s@('M':ss) = parseMRek s
parseFunctionStep ('U':ss) = parseUDef ss
parseFunctionStep _ = throw "Expected Function Prefix"

parseFunctions :: String -> DCont r String ([Function],String)
parseFunctions ('(':ss) = do
  (f,r) <- parseFunctionStep ss
  (fs,r') <- parseFunctionList r
  if head r' == ')' then return (f:fs, tail r') else throw "Expected ')'"
parseFunctions ss = throw "Expected '('"

parseFunctionList :: String -> DCont r String ([Function],String)
parseFunctionList s@(c:ss) | c /= ')' = do
  (f,r) <- parseFunctionStep s
  (fs,r') <- parseFunctionList r
  return (f:fs, r')
parseFunctionList ss = return ([],ss)

parseProj :: String -> DCont r String (Function,String)
parseProj ('P':ss) = parseInt ss >>= (\(i,r) -> return (P i,r))

parseInt :: String -> DCont r String (Int,String)
parseInt [] = throw "Empty String"
parseInt (c:cs) | c >= '0' && '9' >= c = parseIntTail cs >>= (\(i,r) -> return (10^(length cs - length r) * (digitToInt c) + i,r))
parseInt _ = throw "Expected Numeral"

parseIntTail :: String -> DCont r String (Int,String)
parseIntTail [] = return (0,[])
parseIntTail (c:cs) | c >= '0' && '9' >= c = parseIntTail cs >>= (\(i,r) -> return (10^(length cs - length r) * (digitToInt c) + i,r))
parseIntTail cs = return (0,cs)

parseSubs :: String -> DCont r String (Function,String)
parseSubs ('A':ss) = do
  (f,r) <- parseFunctionStep ss
  (fs,r') <- parseFunctions r
  return (Subs f fs,r')

parsePRek :: String -> DCont r String (Function,String)
parsePRek ('R':ss) = do
  (g,r) <- parseFunctionStep ss
  (h,r') <- parseFunctionStep r
  return (PRek g h,r')

parseMRek :: String -> DCont r String (Function,String)
parseMRek ('M':ss) = parseFunctionStep ss >>= (\(g,r) -> return (MRek g, r))

parseUDef:: String -> DCont r String (Function,String)
parseUDef [] = return (UDef [],[])
parseUDef (s:ss) | ('a' <= s && s <= 'z') || ('0' <= s && s <= '9') = parseUDef ss >>= (\(UDef ss',r) -> return (UDef (s:ss'),r))
parseUDef ss = return (UDef [], ss)

parseParams :: String -> [Int]
parseParams [] = []
parseParams x = run (parseInt x) (\(i,r) -> if null r then [i] else i : parseParams (tail r)) (const [])