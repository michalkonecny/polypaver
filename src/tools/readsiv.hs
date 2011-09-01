{-|
    Module      :  Main
    Description :  translator of SPARK VCs to PolyPaver problems 
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Translator of SPARK VCs to PolyPaver problems.
    
    Example usage in a Unix shell:
    
    > runhaskell tools/readsiv.hs test.siv test/
-}
module Main where

import PolyPaver.Paver
import PolyPaver.Form
import PolyPaver.Vars

import System
import Data.Char
import Data.List
import Data.List.Split (splitOn)
import Control.Monad

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Expr
--import Text.Parsec.Prim
import Text.Parsec.Language

main = do
  inputPathS : outputFolder : _ <- getArgs
  fileS <- readFile inputPathS
  let vcSs = vcSplitter fileS
  let vcs = map parseVC vcSs
  let vcBoxes = map (addBox inputPathS) $ filter (notVerum . snd) vcs
  mapM_ (writePolyPaverMain outputFolder) $ vcBoxes
  
addBox inputPathS (name, form) =
    (name, formN, box)
    where
    box =
        case getBox formN of
            Left err -> 
                error $ "readsiv: problem with VC " ++ name ++ ":\n" ++ err
            Right box -> box
    formN = normaliseVars form
  
parseVC s =
    case parse vcParser "vc" s of
        Right t -> t
        Left err -> error $ "parse error: " ++ show err 
        
vcSplitter :: String -> [String]
vcSplitter =  
    filter (not.isSpace.head)
    . filter (not.null)
    . concatMap (splitOn "\n\n")
    . tail
    . concatMap (splitOn "function_")
    . splitOn "procedure_"

vcParser :: Parser (String, Form) 
vcParser = 
    do
    vcName <- m_identifier
    m_dot
    t <- formParser <|> emptyFormParser
    eof
    return (vcName, t)

formParser :: Parser Form
formParser = 
    do
    hs <- many hypothesis
    m_whiteSpace
    m_symbol "->"
    m_whiteSpace
    cs <- many1 conclusion
    return $
        foldr (--->) (foldl1 (/\) cs) hs 

emptyFormParser :: Parser Form
emptyFormParser =
    do
    m_symbol "***"
    manyTill anyToken m_dot 
    return Verum

hypothesis = atomic "H"
conclusion = atomic "C"

atomic symb =
    do
    m_symbol symb
    m_integer
    m_symbol ":"
    m_whiteSpace
    f <- inequality
    m_dot
    return f
    
inequality =
    do
    left <- term
    opF <- op
    right <- term
    return $ opF left right 
    where
    op =
        choice $ map o [("<", Le), ("<=", Leq), (">", Ge), (">=", Geq), ("=", Eq)]
    o (opS, opF) =
        do
        m_reservedOp opS
        return opF

term :: Parser Term
term = buildExpressionParser termTable atomicTerm <?> "term"
termTable = 
    [ [Prefix (m_reservedOp "-" >> return (Neg))]
    , [Infix (m_reservedOp "/" >> return (Over)) AssocLeft]
    , [Infix (m_reservedOp "*" >> return (Times)) AssocLeft]
    , [Infix (m_reservedOp "-" >> return (Minus)) AssocLeft]
    , [Infix (m_reservedOp "+" >> return (Plus)) AssocLeft]
    ]

atomicTerm = m_parens term
        <|> try fncall
        <|> fmap (Lit . fromInteger) m_integer
        <|> fmap var m_identifier
        
fncall =
    do
    fname <- m_identifier
    args <- m_parens $ sepBy term (m_symbol ",")
    return $ decodeFn fname args

-- the following definition is incomplete, add cases as needed:
decodeFn "numeric__divide" [arg1, arg2] = FTimes arg1 arg2
decodeFn "numeric__times" [arg1, arg2] = FTimes arg1 arg2
decodeFn "numeric__plus" [arg1, arg2] = FPlus arg1 arg2
decodeFn "numeric__minus" [arg1, arg2] = FMinus arg1 arg2
decodeFn "exact__sqrt" [arg1] = Sqrt arg1
decodeFn "exact__exp" [arg1] = Exp arg1
decodeFn "exact__sin" [arg1] = Sin arg1
decodeFn "exact__cos" [arg1] = Cos arg1
decodeFn fn args =
    error $ 
        "cannot decode function call " ++ fn ++ 
        "(" ++ (intercalate "," $ map show args) ++ ")"

var name =
    Var $ sum $ zipWith (*) [1..] $ map ord name

tokenDef = emptyDef{ commentStart = "/*"
               , commentEnd = "*/"
               , identStart = letter
               , identLetter = alphaNum <|> (oneOf "_")
               , opStart = oneOf "><=-+*/"
               , opLetter = oneOf "="
               , reservedOpNames = [">=", "<=", "=", "-", "+", "*", "/"]
               }

TokenParser{ parens = m_parens
            , identifier = m_identifier
            , reservedOp = m_reservedOp
            , symbol = m_symbol
            , dot = m_dot
            , integer = m_integer
            , whiteSpace = m_whiteSpace } 
            = 
            makeTokenParser tokenDef

    