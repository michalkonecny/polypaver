{-|
    Module      :  Numeric.ER.Misc
    Description :  general purpose extras 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Miscelaneous utilities (eg related to Ordering, pairs, booleans, strings)
-}
module Numeric.ER.Misc where

import Data.List
import System.IO.Unsafe
import Data.Time.Clock.POSIX

unsafePrint msg val =
    unsafePerformIO $
        do
        putStrLn $ "(unsafe IO) " ++ msg
        return val

unsafePrintReturn msg a =
    unsafePrint (msg ++ show a) a

unsafeReport fileName msg val =
    unsafePerformIO $
        do
        stamp <- getPOSIXTime
        appendFile fileName $ showStamp stamp ++ ":"
        appendFile fileName $ msg ++ "\n"
        return val
    where
    showStamp stamp =
        padTo18 $ show stamp
    padTo18 s = s ++ (replicate (18 - (length s)) ' ')

{-|
    Compose as when defining the lexicographical ordering.
-}
compareCompose :: Ordering -> Ordering -> Ordering
compareCompose EQ o = o
compareCompose o _ = o

{-|
    Compose as when defining the lexicographical ordering.
-}
compareComposeMany :: [Ordering] -> Ordering
compareComposeMany [] = EQ
compareComposeMany (EQ:os) = compareComposeMany os
compareComposeMany (o:_) = o

{-|
    The lexicographical ordering.
-}
compareLex :: (Ord a) => [a] -> [a] -> Ordering
compareLex [] _ = LT
compareLex _ [] = GT
compareLex (x:xs) (y:ys)
    | x == y = compareLex xs ys
    | otherwise = compare x y

compareListsWith ::
    (a -> a -> Ordering) ->
    [a] -> [a] -> Ordering
compareListsWith _ [] [] = EQ
compareListsWith _ [] _ = LT
compareListsWith _ _ [] = GT
compareListsWith compareVals (x:xs) (y:ys) =
    case compareVals x y of
        EQ -> compareListsWith compareVals xs ys
        res -> res

mapFst :: (a1 -> a2) -> (a1,b) -> (a2,b)     
mapFst f (a,b) = (f a,b)
mapSnd :: (b1 -> b2) -> (a,b1) -> (a,b2)     
mapSnd f (a,b) = (a,f b)
mapPair :: (a1 -> a2, b1 -> b2) -> (a1,b1) -> (a2,b2)     
mapPair (f1, f2) (a,b) = (f1 a, f2 b)
mapPairHomog :: (a1 -> a2) -> (a1,a1) -> (a2,a2)     
mapPairHomog f = mapPair (f,f) 

unpair :: [(a,a)] -> [a]
unpair = (\(l1,l2) -> l1 ++ l2) . unzip

bool2maybe :: Bool -> Maybe ()
bool2maybe True = Just ()
bool2maybe False = Nothing

dropLast :: Int -> [a] -> [a]
dropLast n list = reverse $ drop n (reverse list)

{-|
    eg 

>    concatWith "," ["a","b"] = "a,b"

-}
concatWith :: 
    String {-^ a connective -} -> 
    [String] -> 
    String
concatWith sep [] = ""
concatWith sep [str] = str
concatWith sep (str : strs) = str ++ sep ++ (concatWith sep strs)
    
{-|
    eg 

>    replicateSeveral [(2,"a"),(1,"b")] = "aab"

-}
replicateSeveral :: [(Int,a)] -> [a]
replicateSeveral [] = []
replicateSeveral ((n,e):rest) =
    replicate n e ++ (replicateSeveral rest)
    
{-|
    eg 

>    countDuplicates "aaba" = [(2,"a"),(1,"b"),(1,"a")]

-}
countDuplicates :: 
    Eq a => 
    [a] -> 
    [(Int,a)]
countDuplicates list =
    map (\ g -> (length g, head g)) $ group list
    
{-|
    eg
    
>    allCombinations 
>        [
>         (1,['a']), 
>         (2,['b','c']), 
>         (3,['d','e','f'])
>        ] =
>            [
>             [(1,'a'),(2,'b'),(3,'d')], 
>             [(1,'a'),(2,'b'),(3,'e')],
>             [(1,'a'),(2,'b'),(3,'f')],
>             [(1,'a'),(2,'c'),(3,'d')], 
>             [(1,'a'),(2,'c'),(3,'e')],
>             [(1,'a'),(2,'c'),(3,'f')]
>            ]
-}
allCombinations :: 
    [(k,[v])] -> [[(k,v)]]
allCombinations [] = [[]]
allCombinations ((k, vals) : rest) =
    concat $ map (\ v -> map ((k,v):) restCombinations) vals
    where
    restCombinations = 
        allCombinations rest

allPairsCombinations ::
    [(k,(v,v))] -> [[(k,v)]]
allPairsCombinations [] = [[]]
allPairsCombinations ((k, (v1,v2)) : rest) =
    (map ((k, v1) :) restCombinations)
    ++
    (map ((k, v2) :) restCombinations)
    where
    restCombinations =
        allPairsCombinations rest
    
    
{-|
    eg
    
>    allPairsCombinationsEvenOdd 
>        [
>         (1,('a0','a1'), 
>         (2,('b0','b1'), 
>         (3,('c0','c1')
>        ] =
>           ([
>             [(1,'a0'),(2,'b0'),(3,'c0')], 
>             [(1,'a0'),(2,'b1'),(3,'c1')], 
>             [(1,'a1'),(2,'b1'),(3,'c0')], 
>             [(1,'a1'),(2,'b0'),(3,'c1')] 
>            ]
>           ,[
>             [(1,'a0'),(2,'b0'),(3,'c1')], 
>             [(1,'a0'),(2,'b1'),(3,'c0')], 
>             [(1,'a1'),(2,'b0'),(3,'c0')], 
>             [(1,'a1'),(2,'b1'),(3,'c1')] 
>            ]
>           )
-}
allPairsCombinationsEvenOdd ::
    [(k,(v,v))] {-^ the first value is even, the second odd -} -> 
    ([[(k,v)]], [[(k,v)]])
allPairsCombinationsEvenOdd [] = ([[]], [])
allPairsCombinationsEvenOdd ((k, (evenVal,oddVal)) : rest) =
    (
        (map ((k, evenVal) :) restCombinationsEven)
        ++
        (map ((k, oddVal) :) restCombinationsOdd)
    ,
        (map ((k, evenVal) :) restCombinationsOdd)
        ++
        (map ((k, oddVal) :) restCombinationsEven)
    )
    where
    (restCombinationsEven, restCombinationsOdd) =
        allPairsCombinationsEvenOdd rest
    
    
    
{- numeric -}    
    
intLogDown b n = fst $ intLog b n 
intLogUp b n = snd $ intLog b n 
    
intLog ::
    (Num n1, Num n2, Ord n1, Integral n2, Show n1) => 
    n1 {-^ base -} -> 
    n1 {-^ x -} -> 
    (n2, n2)
intLog b n
    | n == 1 = (0,0)
    | n > 1 && n < b = (0,1)
    | n >= b =
        bisect (lgDn, pwDn) (lgUp, pwUp)
    | otherwise = 
        error $ "Numeric.ER.Misc: intLog: illegal argument n = " ++ show n
    where
    ((lgDn, pwDn), (lgUp, pwUp)) = 
        findBounds (1, b) 
        -- lgDn <= log_b n < lgUp; pwDn = b^lgDn; pwUp = b^lgUp
    findBounds (lg, pw)
        | n < pwNext = ((lg, pw), (lgNext, pwNext))
        | otherwise = findBounds (lgNext, pwNext)
        where
        lgNext = 2 * lg
        pwNext = pw * pw
    bisect (lgDn, pwDn) (lgUp, pwUp)
        | pwDn == n = (lgDn, lgDn)
        | pwUp == n = (lgUp, lgUp)
        | lgDn == lgMid = (lgDn, lgUp)
        | lgUp == lgMid = (lgDn, lgUp)
        | n < pwMid =
            bisect (lgDn, pwDn) (lgMid, pwMid)
        | otherwise =
            bisect (lgMid, pwMid) (lgUp, pwUp)
        where
        lgMid = (lgDn + lgUp) `div` 2
        pwMid = pwDn * (b ^ (lgMid - lgDn))
            

{-|
    Directionally rounded versions of @+,*,sum,prod@.
-}
plusUp, plusDown, timesUp, timesDown :: 
    (Num t) =>
    t -> t -> t
divideUp, divideDown :: 
    (Fractional t) =>
    t -> t -> t
sumUp, sumDown, productDown, productUp :: 
    (Num t) =>
    [t] -> t
plusUp = (+)
plusDown c1 c2 = - ((- c1) - c2)
sumUp = foldl plusUp 0
sumDown = foldl plusDown 0
timesUp = (*)
timesDown c1 c2 = - ((- c1) * c2)
productUp = foldl timesUp 1
productDown = foldl timesDown 1
divideUp c1 c2 = c1 / c2
divideDown c1 c2 = - ((- c1) / c2)

{- parsing -}
readMaybe :: (Read a) => String -> Maybe a
readMaybe s =
    case reads s of
        [] -> Nothing
        (val,_) : _ -> Just val

showFirstLastLines ::
    (Show a) => 
    Int {-^ how many initial lines to use -} -> 
    Int {-^ how many final lines to use -} -> 
    a -> 
    String
showFirstLastLines lineCountInit lineCountFinal x 
    | linesTotal > lineCount =
        unlines $ 
            firstLines 
            ++ ["...(" ++ show (linesTotal - lineCount) ++ " lines omitted)..."] ++
            lastLines
    | otherwise = unlines firstLines
    where
    lineCount = lineCountInit + lineCountFinal
    firstLines = take lineCountInit allLines
    lastLines = drop (linesTotal - lineCountFinal) allLines
    allLines = lines $ show x
    linesTotal = length allLines
    
{- sequences -}
listUpdate :: Int -> a -> [a] -> [a]
listUpdate i newx (x:xs) 
    | i == 0 = newx : xs
    | i > 0 = x : (listUpdate (i - 1) newx xs) 


listHasMatch :: (a -> Bool) -> [a] -> Bool
listHasMatch f s =
    foldl (\b a -> b && (f a)) False s
    
--{-| types encoding natural numbers -}
--class TypeNumber n
--    where
--    getTNData :: n
--    getTNNumber :: n -> Int
--
--data TN_0 = TN_0
--tn_0 = TN_0
--data TN_SUCC tn_prev = TN_SUCC tn_prev
--
--type TN_ONE = TN_SUCC TN_0
--tn_1 = TN_SUCC TN_0
--
--instance (TypeNumber TN_0)
--    where
--    getTNData = TN_0
--    getTNNumber _ = 0
--    
--instance 
--    (TypeNumber tn_prev) => 
--    (TypeNumber (TN_SUCC tn_prev))
--    where
--    getTNData = TN_SUCC getTNData
--    getTNNumber (TN_SUCC p) = 1 + (getTNNumber p)
    
