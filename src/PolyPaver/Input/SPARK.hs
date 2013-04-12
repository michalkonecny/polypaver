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
import PolyPaver.DeriveBounds
import PolyPaver.PPBox

import Numeric.ER.Misc

import Data.Char (ord)
import Data.List (intercalate, partition)
import qualified Data.IntMap as IMap

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
    (String, Form, [(Int, (Rational, Rational), Bool)])
    {-^ the VC and the bounding box for its variables -}
parseSivVC sourceDescription s vcName =
    case parse (sivVC vcName) "siv" s of
        Right t -> addBox (vcName, t)
        Left err -> error $ "parse error in " ++ sourceDescription ++ ":" ++ show err 
  
parseSivAll ::
    String {-^ description of the source (eg file name) for error reporting -} ->
    String {-^ the contents of the SPARK vcg or siv file -} -> 
    [(String, Form, [(Int, (Rational, Rational), Bool)])]
    {-^ the VC and the bounding box for its variables -}
parseSivAll sourceDescription s =
    case parse sivAll "siv" s of
        Right t -> map addBox t
        Left err -> error $ "parse error in " ++ sourceDescription ++ ":" ++ show err 
        
addBox (name, form) =
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
            _ -> foldr (--->) (foldl1 (/\) cs) $ sortFormulasBySize hs 

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
    id <- manyTill anyToken (m_symbol ":")
    m_whiteSpace
    f <- formula $ symb ++ id
    m_dot
--    unsafePrint ("vcItem: done item = " ++ symb ++ show n ++ "; form = " ++ showForm f) $ return ()
    return f
    
formula :: Label -> Parser Form
formula lab = buildExpressionParser formTable (atomicFormula lab) <?> ("formula " ++ lab)
formTable = 
    [ [Infix (m_reserved "and" >> return (And)) AssocLeft]
    , [Infix (m_reserved "or" >> return (Or)) AssocLeft]
    , [Infix (m_reservedOp "->" >> return (Implies)) AssocRight]
    ]

atomicFormula lab = 
    (try $ m_parens (formula lab)) 
    <|> 
    (try (inequality lab)) 
    <|> 
    (try (predicate lab))
    
inequality lab =
    do
    left <- term
    opF <- op
    right <- term
    return $ opF lab left right 
    where
    op =
        choice $ map o [("<", Le), ("<=", Leq), (">", Ge), (">=", Geq), ("=", Eq), ("<>", Neq)]
    o (opS, opF) =
        try $
        do
        m_reservedOp opS
        return opF

predicate lab =
    do
    pname <- m_identifier
    args <- m_parens $ sepBy term (m_symbol ",")
    return $ decodePred lab pname args

decodePred lab "num__isint" [arg1] = IsInt lab arg1
decodePred lab "num__isintegerrange" [arg1, arg2, arg3] = IsIntRange lab arg1 arg2 arg3
decodePred lab "num__isfloatrange" [arg1, arg2, arg3] = IsRange lab arg1 arg2 arg3
decodePred lab "num__isdoublerange" [arg1, arg2, arg3] = IsRange lab arg1 arg2 arg3
decodePred lab "exact__contains" [arg1, arg2] = ContainedIn lab arg1 arg2
decodePred lab pred args =
    error $ 
        "in [" ++ lab ++ "], cannot decode predicate " ++ pred ++ 
        "(" ++ (intercalate "," $ map show args) ++ ")"


term :: Parser Term
term = buildExpressionParser termTable atomicTerm <?> "term"
termTable = 
    [ [Prefix (m_reservedOp "-" >> return (termOp1 Neg))]
    , [Infix (m_reservedOp "/" >> return (termOp2 Over)) AssocLeft]
    , [Infix (m_reservedOp "*" >> return (termOp2 Times)) AssocLeft]
    , [Infix (m_reservedOp "-" >> return (termOp2 Minus)) AssocLeft]
    , [Infix (m_reservedOp "+" >> return (termOp2 Plus)) AssocLeft]
    ]

atomicTerm = m_parens term
        <|> try fncall
        <|> fmap fromInteger m_integer
        <|> fmap var m_identifier
        
fncall =
    do
    fname <- m_identifier
    args <- m_parens $ sepBy term (m_symbol ",")
    return $ decodeFn fname args

-- the following definition is incomplete, add cases as needed:
decodeFn "numeric__divide" [arg1, arg2] = arg1 /: arg2
decodeFn "numeric__times" [arg1, arg2] = arg1 *: arg2
decodeFn "numeric__plus" [arg1, arg2] = arg1 +: arg2
decodeFn "numeric__minus" [arg1, arg2] = arg1 -: arg2
decodeFn "num__divide" [arg1, arg2] = arg1 /: arg2
decodeFn "num__multiply" [arg1, arg2] = arg1 *: arg2
decodeFn "num__add" [arg1, arg2] = arg1 +: arg2
decodeFn "num__subtract" [arg1, arg2] = arg1 -: arg2
decodeFn "num__exp" [arg1] = termOp1 FExp arg1
decodeFn "exact__sqrt" [arg1] = sqrt arg1
decodeFn "exact__exp" [arg1] = exp arg1
decodeFn "exact__sin" [arg1] = sin arg1
decodeFn "exact__cos" [arg1] = cos arg1
decodeFn "exact__integral" [arg1, arg2, arg3] = termOp3 (Integral ivNum ivName) arg1 arg2 arg3
decodeFn "abs" [arg1] = abs arg1
decodeFn fn args =
    error $ 
        "cannot decode function call " ++ fn ++ 
        "(" ++ (intercalate "," $ map show args) ++ ")"

var "numeric__epsabs" = fepsAbs
var "numeric__epsrel" = fepsRel
var "num__epsabs" = fepsAbs
var "num__epsrel" = fepsRel
var "exact__integrationvariable" = termVar ivNum ivName
var name =
    termVar n name
    where
    n = sum $ zipWith (*) [1..] $ map ord name

ivNum = -1 
ivName = "<iv>"

tokenDef = emptyDef{ commentStart = "/*"
               , commentEnd = "*/"
               , identStart = letter
               , identLetter = alphaNum <|> (oneOf "_")
               , opStart = oneOf "><=-+*/"
               , opLetter = oneOf "=>"
               , reservedOpNames = [">", "<", ">=", "<=", "=", "<>", "-", "+", "*", "/", "->"]
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

    