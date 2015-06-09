{-|
    Module      :  PolyPaver.Input.Misc
    Description :  miscellaneous parser utilities 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Miscellaneous parser utilities
-}
module PolyPaver.Input.Misc where
import PolyPaver.DeriveBounds (getBox)

import PolyPaver.Form (Form, Term'(Lit), HasDefaultValue)
import PolyPaver.Vars (substituteVarsForm, pruneHypotheses, normaliseVars)


import Text.Parsec (getInput, getPosition, SourcePos, sourceLine, sourceColumn)
import Text.Parsec.String (Parser)

import Data.List (intercalate, partition)
import qualified Data.IntMap as IMap

import Data.Hashable (Hashable)

import Debug.Trace (trace)

traceRuleDoIt ::
    String ->
    Parser a -> Parser a
traceRuleDoIt rulename parser =
    do 
    trace ("v " ++ rulename) $ return ()
--    (try parserAssumeSucceds) <|> parserIfFails
    parserAssumeSucceds
    where
    parserAssumeSucceds =
        do 
        (result, consumed) <- withConsumed parser
        trace ("^ " ++ rulename ++ ": " ++ consumed) $ return result
--    parserIfFails =
--        do
--        trace ("x " ++ rulename) $ parser


{-|
    This function amends the result of a Parsec parser with
    the precise portion of input consumed by the parser.
-}
withConsumed :: Parser a -> Parser (a, String)
withConsumed parser =
    do
    input <- getInput
    start <- getPosition
    result <- parser
    end <- getPosition
    return (result, computeConsumed input start end)

computeConsumed :: String -> SourcePos -> SourcePos -> String
computeConsumed input start end =
--    unsafePrint
--    (
--        "computeConsumed:"
--        ++ "\n input = " ++ show input
--        ++ "\n start = " ++ show start
--        ++ "\n end   = " ++ show end
--        ++ "\n relevantLines = " ++ show relevantLines
--    )
    intercalate "\n" $
    dropLastSegment relevantLines
    where
    relevantLines = 
        take (endLine - startLine + 1) $ unIntercalate '\n' input
    startLine = sourceLine start
    endLine = sourceLine end
    dropLastSegment ls =
        (take (l - 1) ls) ++ [take col $ last ls]
        where
        l = length ls
        col | startLine == endLine = endCol - startCol
            | otherwise = endCol - 1
        startCol = sourceColumn start
        endCol = sourceColumn end

unIntercalate :: (Eq a) => a -> [a] -> [[a]]
unIntercalate sep s = aux [] [] s
    where
    aux doneRev partNext [] = reverse (reverse partNext : doneRev)
    aux doneRev partNext (e : es) 
        | e == sep = aux (reverse partNext : doneRev) [] es
        | otherwise = aux doneRev (e : partNext) es
    

removeDisjointHypothesesAddBox ::
    (Eq l, HasDefaultValue l, Hashable l) =>
    (String, Form l) -> 
    (String, Form l, [(Int, (Rational, Rational), Bool)])
removeDisjointHypothesesAddBox (name, form) =
    (name, formNoSingletonVars, boxNoSingletonVars)
    where
    formNoSingletonVars =
        substituteVarsForm replaceSingletonVarsWithValue formN
        where
        replaceSingletonVarsWithValue varid =
            case IMap.lookup varid boxSingletonVarsMap of
                Just value -> Just $ Lit value
                _ -> Nothing
    boxSingletonVarsMap =
        IMap.fromList $ map (\(varid, (l, _), _) -> (varid, l)) boxSingletonVars 
    (boxSingletonVars, boxNoSingletonVars) = partition isSingleton box
        where
        isSingleton (_varid, (l, r), _isInt) = l == r
    box =
        case getBox formN of
            Left err -> 
                error $ "PolyPaver.Input.SPARK: removeDisjointHypothesesAddBox: problem with VC " ++ name ++ ":\n" ++ err
            Right box2 -> box2
    formN = pruneHypotheses $ normaliseVars form
  
