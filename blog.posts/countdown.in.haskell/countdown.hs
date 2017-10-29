-- Six numbers, add/subtract/multiply/divide
--
-- Expression is one or more Terms, order doesn't matter, but the sign does
-- A Term is one or more Factors
-- A Factor is either, a literal
--                  , or an expression in parentheses

import Data.Char
import Data.Maybe
import Control.Monad
import Control.Lens ((&))

allPartitions :: [a] -> [([a],[a])]
allPartitions [] = [([],[])]
allPartitions [x] = [([x],[]),([],[x])]
allPartitions (x:xs) =  [  (x:l,r  )  | (l,r) <- allPartitions xs ]
                     ++ [  (l  ,x:r)  | (l,r) <- allPartitions xs ]

data Expr       =   ExprT Terms   -- additive expression, i.e. two or more terms separated by +-
                |   ExprF Factors -- multiplicative expression, i.e. two or more factors separated by +-
                |   Single Letter -- where 'Letter' (see below) represents 'a' through 'f', the first six letters
data Terms      = Terms Expr AddOrSubtract Expr         -- second Expr must *not* be ExprT
data Factors    = Factors Expr MultiplyOrDivide Expr    -- second Expr must *not* be ExprF

data AddOrSubtract = Add | Sub
data MultiplyOrDivide = Mul | Div
data Letter = Letter Char -- the Char is between 'a' and 'f' inclusive, for the six allowed variables

letter2offset :: Char -> Int -- 'a'=0, 'b'=1, 'c'=2, ...
letter2offset l = (fromEnum l) - (fromEnum 'a')

-- a 'Show' for Expr. The second of these four bindings below is not entirely
-- necessary. It just means that (a+(b+(c+d))) is printed as (a+b+c+d) instead

instance Show Expr where
    show (Single (Letter l)) = l:[]  -- "a", "b", "c", "d", "e", "f"
    show (ExprT  (Terms e1@(ExprT _) Add e2))  = dropTrailingCloseParenthesis (show e1) ++ "+" ++ show e2 ++ ")"
    show (ExprT  (Terms e1@(ExprT _) Sub e2))  = dropTrailingCloseParenthesis (show e1) ++ "-" ++ show e2 ++ ")"
    show (ExprT  (Terms e1 Add e2))     = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
    show (ExprT  (Terms e1 Sub e2))     = "(" ++ show e1 ++ "-" ++ show e2 ++ ")"
    show (ExprF  (Factors e1 Mul e2))   = show e1 ++ "*" ++ show e2
    show (ExprF  (Factors e1 Div e2))   = show e1 ++ "/" ++ show e2

showWithInputs :: [Int] -> Expr -> String
showWithInputs inputs (Single (Letter l)) = show (inputs !! (letter2offset l))
showWithInputs inputs (ExprT (Terms   e1@(ExprT _) Add e2)) = dropTrailingCloseParenthesis ( showWithInputs inputs e1 ) ++ "+" ++ showWithInputs inputs e2 ++ ")"
showWithInputs inputs (ExprT (Terms   e1@(ExprT _) Sub e2)) = dropTrailingCloseParenthesis ( showWithInputs inputs e1 ) ++ "-" ++ showWithInputs inputs e2 ++ ")"
showWithInputs inputs (ExprT (Terms   e1 Add e2)) = "(" ++  showWithInputs inputs e1 ++ "+" ++ showWithInputs inputs e2 ++ ")"
showWithInputs inputs (ExprT (Terms   e1 Sub e2)) = "(" ++  showWithInputs inputs e1 ++ "-" ++ showWithInputs inputs e2 ++ ")"
showWithInputs inputs (ExprF (Factors e1 Mul e2)) =         showWithInputs inputs e1 ++ "*" ++ showWithInputs inputs e2
showWithInputs inputs (ExprF (Factors e1 Div e2)) =         showWithInputs inputs e1 ++ "/" ++ showWithInputs inputs e2

dropTrailingCloseParenthesis ")" = ""
dropTrailingCloseParenthesis (x:xs) = x : dropTrailingCloseParenthesis xs

allExprs :: [Letter] -> [Expr]
allExprs [] = []
allExprs [l] = [Single l]
allExprs ls =   [ ExprT (Terms e1 addOrSubtract e2)
                | (l,r) <- allPartitions ls
                , not . null $ l
                , not . null $ r
                , e2 <- allExprs r
                , case e2 of -- to enforce that the second Expr in an ExprT is *not* an ExprT
                        (ExprT _)   -> False
                        _           -> True
                , e1 <- allExprs l
                , addOrSubtract <- [Add, Sub]
                ]
             ++ [ ExprF (Factors e1 multiplyOrDivide e2)
                | (l,r) <- allPartitions ls
                , not . null $ l
                , not . null $ r
                , e2 <- allExprs r
                , case e2 of -- to enforce that the second Expr in an ExprF is *not* an ExprF
                        (ExprF _)   -> False
                        _           -> True
                , e1 <- allExprs l
                , multiplyOrDivide <- [Mul, Div]
                ]

{-
 -  allPartialExprs:
 -  Very similar to allExprs, but allowing a subset of letters to be used
 -}
allPartialExprs :: [Letter] -> [Expr]
allPartialExprs ls  =   [ es
                        | (used, ignored) <- allPartitions ls
                        , not . null $ used
                        , es <- allExprs used
                        ]

eval :: [Int] -> Expr -> Maybe Int
eval is (Single (Letter l))                  = do
                                        return (is !! (letter2offset l))
eval is (ExprT (Terms   e1 addOrSubtract e2))   = do
                                            t1 <- eval is e1
                                            t2 <- eval is e2
                                            case addOrSubtract of
                                                Add -> return (t1+t2)
                                                Sub -> return (t1-t2)
eval is (ExprF (Factors e1 Mul e2))   = do
                                            f1 <- eval is e1
                                            f2 <- eval is e2
                                            return (f1*f2)
eval is (ExprF (Factors e1 Div e2))   = do
                                            f1 <- eval is e1
                                            f2 <- eval is e2
                                            guard $ f2 /= 0
                                            guard ( f1 `mod` f2 == 0 ) -- integer division mustn't have a remainder
                                            return (f1 `div` f2)
{-
eval is (ExprT (Terms   e1 Add e2))   = Just $ (eval is e1) + (eval is e2)
eval is (ExprT (Terms   e1 Sub e2))   = Just $ (eval is e1) - (eval is e2)
eval is (ExprF (Factors e1 Mul e2))   = Just $ (eval is e1) * (eval is e2)
eval is (ExprF (Factors e1 Div e2))   = if numerator `mod` denominator == 0
                                        then Just $ numerator `div` denominator
                                        else Nothing
                where numerator         = eval is e1
                      denominator     = eval is e2
                      -}

--findSolutions :: Int -> [Int] -> [Expr]
findSolutions target inputs =   do
                                    let range_of_chars = ['a' .. (toEnum . (subtract 1) . (+ (length inputs)) . fromEnum $ 'a')]
                                    -- if (length inputs == 6), then range_of_chars will be ['a'..'f']
                                    e <- allPartialExprs $ map Letter range_of_chars
                                    guard . (== (Just target))  $ (eval inputs e)
                                    return (e, showWithInputs inputs e)

findSolutionsAndPrintThemNicely target inputs =
    findSolutions target inputs &
                mapM (\(e,filledIn) ->
                    do
                        let showe = show e
                        let padding = replicate (24 - length showe) ' '
                        putStrLn (padding ++ showe ++ " " ++ filledIn)
                )


main = do
    -- findSolutionsAndPrintThemNicely 2 [10,2,6]
    findSolutionsAndPrintThemNicely 562 [9,8,2,4,10,4]
