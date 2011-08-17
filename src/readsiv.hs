module Main where

import Form

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
  pathS : _ <- getArgs
  fileS <- readFile pathS
  let terms = map parseVC (vcs fileS)
  mapM_ print terms 

parseVC s =
    case parse termParser "term" s of -- TODO, change this to vcParser
        Right t -> t
        Left err -> error $ "parse error: " ++ show err 
        
vcs :: String -> [String]
vcs =  
    intersperse "DELIMETER"
    . filter (not.isSpace.head)
    . filter (not.null)
    . concatMap (splitOn "\n\n")
    . tail
    . splitOn "function_"

--vc :: Parser Form 
--vc = 
--    do

tokenDef = emptyDef{ commentStart = "/*"
               , commentEnd = "*/"
               , identStart = letter
               , identLetter = alphaNum
               , opStart = oneOf "><=-+*/"
               , opLetter = oneOf "="
               , reservedOpNames = [">=", "<=", "=", "-", "+", "*", "/"]
               }

TokenParser{ parens = m_parens
            , identifier = m_identifier
            , reservedOp = m_reservedOp
            , reserved = m_reserved
            , symbol = m_symbol
            , semiSep1 = m_semiSep1
            , whiteSpace = m_whiteSpace } 
            = 
            makeTokenParser tokenDef

termParser :: Parser Term
termParser = buildExpressionParser termTable term <?> "term"
termTable = 
    [ [Prefix (m_reservedOp "-" >> return (Neg))]
    , [Infix (m_reservedOp "-" >> return (Minus)) AssocLeft]
    , [Infix (m_reservedOp "+" >> return (Plus)) AssocLeft]
    , [Infix (m_reservedOp "*" >> return (Times)) AssocLeft]
    , [Infix (m_reservedOp "/" >> return (Over)) AssocLeft]
    ]

term = m_parens termParser
        <|> try fncall
        <|> fmap var m_identifier
        
fncall =
    do
    fname <- m_identifier
    args <- m_parens $ sepBy termParser (m_symbol ",")
    return $ decodeFn fname args

decodeFn "numeric__times" [arg1, arg2] = FTimes arg1 arg2
decodeFn "numeric__plus" [arg1, arg2] = FPlus arg1 arg2
decodeFn "numeric__minus" [arg1, arg2] = FMinus arg1 arg2
decodeFn "exact__sqrt" [arg1] = Sqrt arg1
decodeFn fn args =
    error $ 
        "cannot decode function call " ++ fn ++ 
        "(" ++ (intercalate "," $ map show args) ++ ")"

var name =
    Var $ sum $ zipWith (*) [1..] $ map ord name
    