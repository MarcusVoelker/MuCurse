module Function(Function(S,C,P,Subs,PRek,MRek,UDef),FunctionDef(FunctionDef),evaluate,resolveProgram) where

import Data.List

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
    show (MRek g) = "M" ++ show g
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