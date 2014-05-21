module Main where

import Data.List
import Data.Char
import Data.Maybe
import System.Environment

data Function = S
              | C
              | P Int
              | Subs Function [Function]
              | PRek Function Function
              | MRek Function
              | UDef String

data FunctionDef = FunctionDef String Function

instance Show Function where
    show S = "S"
    show C = "C"
    show (P i) = "P" ++ show i
    show (Subs g hs) = show g ++ "(" ++ (intercalate "," (map show hs)) ++ ")"
    show (PRek g h) = "[PRek g:" ++ show g ++ ", h:" ++ show h ++ "]"
    show (MRek g) = "µ" ++ show g
    show (UDef s) = s

instance Show FunctionDef where
    show (FunctionDef n f) = n ++ " = " ++ show f

evaluate :: Function -> [Int] -> Int
evaluate S [v] = 1 + v
evaluate C _   = 0
evaluate (P i) vs = vs !! i
evaluate (Subs g hs) vs = evaluate g (map (\f -> evaluate f vs) hs)
evaluate (PRek g h) vs | last vs == 0 = evaluate g $ init vs
                       | otherwise    = evaluate h (dec ++ [evaluate (PRek g h) dec])
                        where dec = (init vs) ++ [last vs - 1]
evaluate (MRek g) vs = mrek g vs 0

mrek :: Function -> [Int] -> Int -> Int
mrek g vs i | evaluate g (vs ++[i]) == 0 = i
            | otherwise                  = mrek g vs (i+1)

runProgram :: [String] -> [Int] -> Int
runProgram defs vs = evaluate (resolveProgram (parseFunctionDefs defs)) vs

resolveProgram :: [FunctionDef] -> Function
resolveProgram defs = (\(FunctionDef _ f) -> f) $ head $ filter (\(FunctionDef n _) -> n == "main") (resolveFunctionDefs defs)

resolveFunction :: [FunctionDef] -> Function -> Function
resolveFunction defs (Subs g hs) = Subs (resolveFunction defs g) (map (resolveFunction defs) hs)
resolveFunction defs (PRek g h) = PRek (resolveFunction defs g) (resolveFunction defs h)
resolveFunction defs (MRek g) = MRek (resolveFunction defs g)
resolveFunction defs (UDef s) = resolveFunction defs $  (\(FunctionDef n f) -> f) $ head (filter (\(FunctionDef n f) -> s == n) defs)
resolveFunction defs atom = atom

resolveFunctionDefs :: [FunctionDef] -> [FunctionDef]
resolveFunctionDefs defs = map (\(FunctionDef n f) -> FunctionDef n $ resolveFunction defs f) defs

parseFunctionDefs :: [String] -> [FunctionDef]
parseFunctionDefs ss = catMaybes (map tryParseFunctionDef ss)

tryParseFunctionDef :: String -> Maybe FunctionDef
tryParseFunctionDef s = parseFunctionDef s Just (const Nothing)

tryParseFunction :: String -> Maybe Function
tryParseFunction s = parseFunction s Just (const Nothing)

parseFunctionDef :: String -> (FunctionDef -> r) -> (String -> r) -> r
parseFunctionDef ss ok err = parseUDef ss (\(UDef s,r) -> if (head r == '=') then parseFunction (tail r) (\f -> ok (FunctionDef s f)) err else err "Expected '='") err

parseFunction :: String -> (Function -> r) -> (String -> r) -> r
parseFunction = (. (. fst)) . parseFunctionStep

parseFunctionStep :: String -> ((Function,String) -> r) -> (String -> r) -> r
parseFunctionStep [] _ err = err "Empty String"
parseFunctionStep ('S':ss) ok _ = ok (S,ss)
parseFunctionStep ('C':ss) ok _ = ok (C,ss)
parseFunctionStep s@('P':ss) ok err = parseProj s ok err
parseFunctionStep s@('A':ss) ok err = parseSubs s ok err
parseFunctionStep s@('R':ss) ok err = parsePRek s ok err
parseFunctionStep s@('M':ss) ok err = parseMRek s ok err
parseFunctionStep ('U':ss) ok err = parseUDef ss ok err
parseFunctionStep _ _ err = err "Expected Function Prefix"

parseFunctions :: String -> (([Function],String) -> r) -> (String -> r) -> r
parseFunctions ('(':ss) ok err = parseFunctionStep ss (\(f,r) -> parseFunctionList r (\(fs,r') -> if head r' == ')' then ok (f:fs, tail r') else err "Expected ')'") err) err
parseFunctions ss _ err = err "Expected '('"

parseFunctionList :: String -> (([Function],String) -> r) -> (String -> r) -> r
parseFunctionList s@(c:ss) ok err | c /= ')' = parseFunctionStep s (\(f,r) -> parseFunctionList r (\(fs,r') -> ok (f:fs, r')) err) err
parseFunctionList ss ok _ = ok ([],ss)

parseProj :: String -> ((Function,String) -> r) -> (String -> r) -> r
parseProj ('P':ss) ok err = parseInt ss (\(i,r) -> ok (P i,r)) err

parseInt :: String -> ((Int,String) -> r) -> (String -> r) -> r
parseInt [] _ err = err "Empty String"
parseInt (c:cs) ok err | c >= '0' && '9' >= c = parseIntTail cs (\(i,r) -> ok (10^(length cs - length r) * (digitToInt c) + i,r)) err
parseInt _ _ err = err "Expected Numeral"

parseIntTail :: String -> ((Int,String) -> r) -> (String -> r) -> r
parseIntTail [] ok _ = ok (0,[])
parseIntTail (c:cs) ok err | c >= '0' && '9' >= c = parseIntTail cs (\(i,r) -> ok (10^(length cs - length r) * (digitToInt c) + i,r)) err
parseIntTail cs ok _ = ok (0,cs)

parseSubs :: String -> ((Function,String) -> r) -> (String -> r) -> r
parseSubs ('A':ss) ok err = parseFunctionStep ss (\(f,r) -> parseFunctions r (\(fs,r') -> ok (Subs f fs,r')) err) err

parsePRek :: String -> ((Function,String) -> r) -> (String -> r) -> r
parsePRek ('R':ss) ok err = parseFunctionStep ss (\(g,r) -> parseFunctionStep r (\(h,r') -> ok (PRek g h,r')) err) err

parseMRek :: String -> ((Function,String) -> r) -> (String -> r) -> r
parseMRek ('M':ss) ok err = parseFunctionStep ss (\(g,r) -> ok (MRek g, r)) err

parseUDef:: String -> ((Function,String) -> r) -> (String -> r) -> r
parseUDef [] ok _ = ok (UDef [],[])
parseUDef (s:ss) ok err | ('a' <= s && s <= 'z') || ('0' <= s && s <= '9') = parseUDef ss (\(UDef ss',r) -> ok (UDef (s:ss'),r)) err
parseUDef ss ok _ = ok (UDef [], ss)

parseParams :: String -> [Int]
parseParams [] = []
parseParams x = parseInt x (\(i,r) -> if null r then [i] else i : parseParams (tail r)) (const [])

main :: IO ()
main = do
    path <- getArgs
    text <- readFile $ head path
    let l = lines text
    let p = runProgram l
    params <- getLine
    let i = p (parseParams params)
    putStrLn ("Output is " ++ show i)
