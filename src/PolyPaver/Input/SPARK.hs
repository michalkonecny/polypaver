{-|
    Module      :  Main
    Description :  parser of SPARK vcg and siv files 
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Parser of SPARK vcg and siv files.
-}
module PolyPaver.Input.SPARK 
(
    parseSivVC,
    parseSivAll
)
where

import PolyPaver.GenerateMain
import PolyPaver.Form
import PolyPaver.Vars
import PolyPaver.PPBox

import Numeric.ER.Misc

import Data.Char (ord)
import Data.List (intercalate)

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Expr
--import Text.Parsec.Prim
import Text.Parsec.Language

--main = do
--  inputPathS : outputFolder : _ <- getArgs
--  fileS <- readFile inputPathS
--  let vcs = parseSiv inputPathS fileS
--  let vcBoxes = map (addBox inputPathS) $ filter (notVerum . snd) vcs
--  mapM_ (writePolyPaverMain outputFolder) $ vcBoxes
  
parseSivVC ::
    String {-^ description of the source (eg file name) for error reporting -} ->
    String {-^ the contents of the SPARK vcg or siv file -} -> 
    String {-^ VC name -} -> 
    (String, Form, [(Int, (Rational, Rational))])
    {-^ the VC and the bounding box for its variables -}
parseSivVC sourceDescription s vcName =
    case parse (sivVC vcName) "siv" s of
        Right t -> addBox (vcName, t)
        Left err -> error $ "parse error in " ++ sourceDescription ++ ":" ++ show err 
  
parseSivAll ::
    String {-^ description of the source (eg file name) for error reporting -} ->
    String {-^ the contents of the SPARK vcg or siv file -} -> 
    [(String, Form, [(Int, (Rational, Rational))])]
    {-^ the VC and the bounding box for its variables -}
parseSivAll sourceDescription s =
    case parse sivAll "siv" s of
        Right t -> map addBox t
        Left err -> error $ "parse error in " ++ sourceDescription ++ ":" ++ show err 
        
addBox (name, form) =
    (name, formN, box)
    where
    box =
        case getBox formN of
            Left err -> 
                error $ "PolyPaver.Input.SPARK: addBox: problem with VC " ++ name ++ ":\n" ++ err
            Right box -> box
    formN = removeDisjointHypotheses $ normaliseVars form
  
sivAll :: Parser [(String, Form)]
sivAll =
    many $
    do
    untilStartOfVC
    vcName <- vcHead
    form <- vcWhole
    return (vcName, form)
    where
    untilStartOfVC =
        (try startOfVC) <|> (manyTill anyToken $ try $ do { newline; startOfVC })
    startOfVC =
        (m_symbol $ "procedure_")
        <|> 
        (m_symbol $ "function_")
  
sivVC :: String -> Parser Form
sivVC vcName =
    do
    untilStartOfVC
    m_dot
    vcWhole
    where
    untilStartOfVC =
        (try startOfVC) <|> (manyTill anyToken $ try $ do { newline; startOfVC })
    startOfVC =
        (m_symbol $ "procedure_" ++ vcName)
        <|> 
        (m_symbol $ "function_" ++ vcName)

vcHead =
    do
    vcName <- m_identifier
    m_dot
    return vcName

vcWhole :: Parser Form 
vcWhole = 
    do
    t <- vcBody <|> vcEmptyBody
--    unsafePrint ("vcWhole: done vcName = " ++ vcName ++ "; form = " ++ showForm t) $ return ()
    return t

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
decodeFn "abs" [arg1] = Abs arg1
decodeFn fn args =
    error $ 
        "cannot decode function call " ++ fn ++ 
        "(" ++ (intercalate "," $ map show args) ++ ")"

var "numeric__epsabs" = EpsAbs
var "numeric__epsrel" = EpsRel
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

    