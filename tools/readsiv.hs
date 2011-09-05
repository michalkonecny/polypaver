{-|
    Module      :  Main
    Description :  translator of SPARK VCs to PolyPaver problems 
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Translator of SPARK VCs to PolyPaver problems.
-}
module Main where

import PolyPaver.GenerateMain
import PolyPaver.Form
import PolyPaver.Vars

import Numeric.ER.Misc

import System.Environment (getArgs)
import Data.Char (ord)
import Data.List (intercalate)

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Expr
--import Text.Parsec.Prim
import Text.Parsec.Language

main = do
  inputPathS : outputFolder : _ <- getArgs
  fileS <- readFile inputPathS
  let vcs = parseSiv inputPathS fileS
  let vcBoxes = map (addBox inputPathS) $ filter (notVerum . snd) vcs
  mapM_ (writePolyPaverMain outputFolder) $ vcBoxes
  
addBox inputPathS (name, form) =
    (name, removeDisjointHypotheses formN, box)
    where
    box =
        case getBox formN of
            Left err -> 
                error $ "addBox: problem with VC " ++ name ++ ":\n" ++ err
            Right box -> box
    formN = normaliseVars form
  
parseSiv filePath s =
    case parse siv "siv" s of
        Right t -> t
        Left err -> error $ "parse error in file " ++ filePath ++ ":" ++ show err 
        
siv :: Parser [(String, Form)]
siv =
    many $
        do
        untilStartOfVC
        vcWhole
        where
        untilStartOfVC =
            (try startOfVC) <|> (manyTill anyToken $ try $ do { newline; startOfVC })
        startOfVC =
            (m_symbol "procedure_")
            <|> 
            (m_symbol "function_")

vcHead =
    do
    vcName <- m_identifier
--    unsafePrint ("vcHead: got identifier = " ++ vcName) $ return ()
    m_dot
--    unsafePrint ("vcHead: vcName = " ++ vcName) $ return ()
    return vcName

vcWhole :: Parser (String, Form) 
vcWhole = 
    do
    vcName <- vcHead
    t <- vcBody <|> vcEmptyBody
--    unsafePrint ("vcWhole: done vcName = " ++ vcName ++ "; form = " ++ showForm t) $ return ()
    return (vcName, t)

vcBody :: Parser Form
vcBody = 
    do
    hs <- many hypothesis
    m_whiteSpace
    m_reservedOp "->"
    m_whiteSpace
    cs <- many (try conclusion)
    return $
        case cs of
            [] -> Verum
            _ -> foldr (--->) (foldl1 (/\) cs) hs 

vcEmptyBody :: Parser Form
vcEmptyBody =
    do
    m_symbol "***"
    manyTill anyToken m_dot 
    return Verum

hypothesis = vcItem "H"
conclusion = vcItem "C"

vcItem symb =
    do
    m_symbol symb
    n <- m_integer
    m_symbol ":"
    m_whiteSpace
    f <- formula
    m_dot
--    unsafePrint ("vcItem: done item = " ++ symb ++ show n ++ "; form = " ++ showForm f) $ return ()
    return f
    
formula :: Parser Form
formula = buildExpressionParser formTable atomicFormula <?> "formula"
formTable = 
    [ [Infix (m_reserved "and" >> return (And)) AssocLeft]
    , [Infix (m_reserved "or" >> return (Or)) AssocLeft]
    , [Infix (m_reservedOp "->" >> return (Implies)) AssocRight]
    ]

atomicFormula = (try $ m_parens formula) <|> inequality
    
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
        try $
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
    Var n name
    where
    n = sum $ zipWith (*) [1..] $ map ord name

tokenDef = emptyDef{ commentStart = "/*"
               , commentEnd = "*/"
               , identStart = letter
               , identLetter = alphaNum <|> (oneOf "_")
               , opStart = oneOf "><=-+*/"
               , opLetter = oneOf "=>"
               , reservedOpNames = [">", "<", ">=", "<=", "=", "-", "+", "*", "/", "->"]
               , reservedNames = ["and", "or", "implies"]
               }

TokenParser{ parens = m_parens
            , identifier = m_identifier
            , reservedOp = m_reservedOp
            , reserved = m_reserved
            , symbol = m_symbol
            , dot = m_dot
            , integer = m_integer
            , whiteSpace = m_whiteSpace } 
            = 
            makeTokenParser tokenDef

    