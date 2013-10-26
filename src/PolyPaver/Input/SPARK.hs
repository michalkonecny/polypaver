{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
--{-# LANGUAGE RankNTypes #-}

{-|
    Module      :  PolyPaver.Input.SPARK
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
    parseVCInFile,
    parseSivVC,
    parseSivAll
)
where

import PolyPaver.Form
import PolyPaver.Vars
import PolyPaver.DeriveBounds

import Numeric.ER.Misc
--import Numeric.ER.Real.DefaultRepr

import Data.Char (ord, isSpace)
import Data.List (intercalate, partition)
import qualified Data.IntMap as IMap


import Data.Functor.Identity
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
  
parseVCInFile ::
    String {-^ description of the source (eg file name) for error reporting -} ->
    String {-^ the contents of the vc file -} -> 
    (String, Form (), [(Int, (Rational, Rational), Bool)])
    {-^ the VC and the bounding box for its variables -}
parseVCInFile sourceDescription s =
    case parse vcInFile sourceDescription s of
        Right (vcName, t) -> addBox (vcName, t)
        Left err -> error $ "parse error in " ++ sourceDescription ++ ":" ++ show err 
  
parseSivVC ::
    String {-^ description of the source (eg file name) for error reporting -} ->
    String {-^ the contents of the SPARK vcg or siv file -} -> 
    String {-^ VC name -} -> 
    (String, Form (), [(Int, (Rational, Rational), Bool)])
    {-^ the VC and the bounding box for its variables -}
parseSivVC sourceDescription s vcName =
    case parse (sivVC vcName) "siv" s of
        Right t -> addBox (vcName, t)
        Left err -> error $ "parse error in " ++ sourceDescription ++ ":" ++ show err 
  
parseSivAll ::
    String {-^ description of the source (eg file name) for error reporting -} ->
    String {-^ the contents of the SPARK vcg or siv file -} -> 
    [(String, Form (), [(Int, (Rational, Rational), Bool)])]
    {-^ the VC and the bounding box for its variables -}
parseSivAll sourceDescription s =
    case parse sivAll "siv" s of
        Right t -> map addBox t
        Left err -> error $ "parse error in " ++ sourceDescription ++ ":" ++ show err 
        
addBox ::
    (Eq l, HasDefaultValue l) =>
    (String, Form l) -> 
    (String, Form l, [(Int, (Rational, Rational), Bool)])
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
            Right box2 -> box2
    formN = removeDisjointHypotheses $ normaliseVars form
  
sivAll :: Parser [(String, Form ())]
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
  
sivVC :: String -> Parser (Form ())
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

vcInFile :: Parser (String, Form ())
vcInFile =
    do
    m_whiteSpace
    name <- m_identifier
    m_dot
    form <- vcWhole
    m_whiteSpace
    eof
    return (name, form)

vcHead :: Parser String
vcHead =
    do
    vcName <- m_identifier
    m_dot
    return vcName

vcWhole :: Parser (Form ()) 
vcWhole = 
    do
    t <- vcHypothesesAndConclusions <|> vcConclusionsOnly <|> vcEmpty
--    unsafePrint ("vcWhole: done vcName = " ++ vcName ++ "; form = " ++ showForm t) $ return ()
    return t

vcHypothesesAndConclusions :: Parser (Form ())
vcHypothesesAndConclusions = 
    do
    hs <- many hypothesis
    m_whiteSpace
    m_reservedOp "->"
    m_whiteSpace
    cs <- many1 (try conclusion)
    return $ foldr (--->) (foldl1 (/\) cs) $ sortFormulasBySize hs 

vcConclusionsOnly :: Parser (Form ())
vcConclusionsOnly = 
    do
    cs <- many1 (try conclusion)
    return $ foldl1 (/\) cs


vcEmpty :: Parser (Form ())
vcEmpty =
    do
    m_symbol "***"
    manyTill anyToken m_dot 
    return verum

hypothesis :: Parser (Form ())
hypothesis = vcItem "H"
conclusion :: Parser (Form ())
conclusion = vcItem "C"

vcItem :: String -> Parser (Form ())
vcItem symb =
    do
    m_symbol symb
    vcId <- manyTill anyToken (m_symbol ":")
    m_whiteSpace
    f <- formula $ symb ++ vcId
    m_dot
--    unsafePrint ("vcItem: done item = " ++ symb ++ show n ++ "; form = " ++ showForm f) $ return ()
    return f
    
formula :: FormLabel -> Parser (Form ())
formula lab = buildExpressionParser formTable (atomicFormula lab) <?> ("formula " ++ lab)

formTable :: [[Operator String () Identity (Form l)]]
formTable = 
    [ [Infix (m_reserved "and" >> return (And)) AssocLeft]
    , [Infix (m_reservedOp "/\\" >> return (And)) AssocLeft]
    , [Infix (m_reservedOp "&&" >> return (And)) AssocLeft]
    , [Infix (m_reservedOp "&" >> return (And)) AssocLeft]
    , [Infix (m_reserved "or" >> return (Or)) AssocLeft]
    , [Infix (m_reservedOp "\\/" >> return (Or)) AssocLeft]
    , [Infix (m_reservedOp "||" >> return (Or)) AssocLeft]
    , [Infix (m_reservedOp "|" >> return (Or)) AssocLeft]
    , [Infix (m_reserved "implies" >> return (Implies)) AssocRight]
    , [Infix (m_reservedOp "->" >> return (Implies)) AssocRight]
    ]

atomicFormula :: FormLabel -> Parser (Form ())
atomicFormula lab = 
    (try $ m_parens (formula lab)) 
    <|> 
    (try (inequality lab)) 
    <|> 
    (try (predicate lab))
    
inequality :: FormLabel -> Parser (Form ())
inequality lab =
    do
    left <- term
    opF <- op
    right <- term
    return $ opF lab left right 
    where
    op =
        choice $ map o 
            [("<", Le), ("<=", Leq), (">", Ge), (">=", Geq), 
             ("=", Eq), ("==", Eq),
             ("<>", Neq), ("!=", Neq), ("/=", Neq),
             ("<-", ContainedIn), ("|<-|", ContainedIn)]
    o (opS, opF) =
        try $
        do
        m_reservedOp opS
        return opF

predicate :: FormLabel -> Parser (Form ())
predicate lab =
    do
    pname <- m_identifier
    args <- m_parens $ sepBy term (m_symbol ",")
    return $ decodePred lab pname args

decodePred :: FormLabel -> String -> [Term l] -> Form l
decodePred lab "polypaver__integers__is_integer" [arg1] = IsInt lab arg1
decodePred lab "polypaver__integers__is_range" [arg1, arg2, arg3] = IsIntRange lab arg1 arg2 arg3
decodePred lab "polypaver__floats__is_range" [arg1, arg2, arg3] = IsRange lab arg1 arg2 arg3
decodePred lab "polypaver__long_floats__is_range" [arg1, arg2, arg3] = IsRange lab arg1 arg2 arg3
decodePred lab "polypaver__interval__contained_in" [arg1, arg2] = ContainedIn lab arg1 arg2

decodePred lab "num__isint" [arg1] = IsInt lab arg1
decodePred lab "num__isintegerrange" [arg1, arg2, arg3] = IsIntRange lab arg1 arg2 arg3
decodePred lab "num__isfloatrange" [arg1, arg2, arg3] = IsRange lab arg1 arg2 arg3
decodePred lab "num__isdoublerange" [arg1, arg2, arg3] = IsRange lab arg1 arg2 arg3
decodePred lab "exact__containedin" [arg1, arg2] = ContainedIn lab arg1 arg2

decodePred lab "integer" [arg1] = IsInt lab arg1

decodePred lab pred2 args =
    error $ 
        "in [" ++ lab ++ "], cannot decode predicate " ++ pred2 ++ 
        "(" ++ (intercalate "," $ map show args) ++ ")"


term :: Parser (Term ())
term = buildExpressionParser termTable atomicTerm <?> "term"

termTable :: [[Operator String () Identity (Term ())]]
termTable = 
    [ [prefix "-" negate]
    , [binary "^" (termOp2 IntPower) AssocLeft]
    , [binary "/" (/) AssocLeft] ++ (binaryV ["(/)","/:"] (/:) AssocLeft)
    , [binary "*" (*) AssocLeft] ++ (binaryV ["(*)","*:"] (*:) AssocLeft)
    , [binary "-" (-) AssocLeft] ++ (binaryV ["(-)","-:"] (-:) AssocLeft)
    , [binary "+" (+) AssocLeft] ++ (binaryV ["(+)","+:"] (+:) AssocLeft)
    ]
    where
    binaryV names fun assoc = map (\name -> binary name fun assoc) names
    binary name fun assoc = Infix (do{ m_reservedOp name; return fun }) assoc
    prefix name fun = Prefix (do{ m_reservedOp name; return fun })
    
atomicTerm :: Parser (Term ()) 
atomicTerm 
    = m_parens term
    <|> absBrackets term
    <|> try fncall
    <|> (try $ fmap (fromRational . toRational) m_float)
    <|> fmap (fromInteger) m_integer
    <|> fmap var m_identifier
    <|> interval_literal
        
absBrackets ::
     Num b =>
     Parser b -> Parser b
absBrackets p =
    do
    m_symbol "|"
    res <- p
    m_symbol "|"
    return $ abs res
        
interval_literal :: Parser (Term ())
interval_literal =
    do
    m_symbol "["
    lb <- term
    (m_symbol "," <|> m_symbol "..")
    ub <- term
    m_symbol "]"
    return $ hull lb ub
        
fncall :: Parser (Term ())
fncall =
    do
    ((fname, args), original) <- withConsumed $
        do
        fname <- m_identifier
        args <- m_parens $ sepBy term (m_symbol ",")
        return (fname, args)
    return $ decodeFn original fname args


-- functions used in hand-written VC-style problems:
decodeFn :: 
    String -> String -> [Term ()] -> Term ()
decodeFn _ "Hull" [arg1, arg2] = hull arg1 arg2
decodeFn _ "Interval" [arg1, arg2] = hull arg1 arg2
decodeFn _ "Sqrt" [arg1] = sqrt arg1
decodeFn _ "Exp" [arg1] = exp arg1
decodeFn _ "Sin" [arg1] = sin arg1
decodeFn _ "Cos" [arg1] = cos arg1
decodeFn original "Integral" args = decodeIntegral original args

-- functions declared in the PolyPaver SPARK package and its sub-packages:
decodeFn _ "polypaver__floats__divide" [arg1, arg2] = termOp2 (FOver 24 126) arg1 arg2
decodeFn _ "polypaver__floats__multiply" [arg1, arg2] = termOp2 (FTimes 24 126) arg1 arg2
decodeFn _ "polypaver__floats__add" [arg1, arg2] = termOp2 (FPlus 24 126) arg1 arg2
decodeFn _ "polypaver__floats__subtract" [arg1, arg2] = termOp2 (FMinus 24 126) arg1 arg2
decodeFn _ "polypaver__floats__square" [arg1] = termOp1 (FSquare 24 126) arg1
decodeFn _ "polypaver__floats__sqrt" [arg1] = termOp1 (FSqrt 24 126) arg1
decodeFn _ "polypaver__floats__sin" [arg1] = termOp1 (FSin 24 126) arg1
decodeFn _ "polypaver__floats__cos" [arg1] = termOp1 (FCos 24 126) arg1
decodeFn _ "polypaver__floats__exp" [arg1] = termOp1 (FExp 24 126) arg1

decodeFn _ "polypaver__long_floats__divide" [arg1, arg2] = termOp2 (FOver 53 1022) arg1 arg2
decodeFn _ "polypaver__long_floats__multiply" [arg1, arg2] = termOp2 (FTimes 53 1022) arg1 arg2
decodeFn _ "polypaver__long_floats__add" [arg1, arg2] = termOp2 (FPlus 53 1022) arg1 arg2
decodeFn _ "polypaver__long_floats__subtract" [arg1, arg2] = termOp2 (FMinus 53 1022) arg1 arg2
decodeFn _ "polypaver__long_floats__square" [arg1] = termOp1 (FSquare 53 1022) arg1
decodeFn _ "polypaver__long_floats__sqrt" [arg1] = termOp1 (FSqrt 53 1022) arg1
decodeFn _ "polypaver__long_floats__sin" [arg1] = termOp1 (FSin 53 1022) arg1
decodeFn _ "polypaver__long_floats__cos" [arg1] = termOp1 (FCos 53 1022) arg1
decodeFn _ "polypaver__long_floats__exp" [arg1] = termOp1 (FExp 53 1022) arg1

decodeFn _ "polypaver__interval__hull" [arg1, arg2] = hull arg1 arg2
decodeFn _ "polypaver__interval__interval" [arg1, arg2] = hull arg1 arg2

decodeFn _ "polypaver__exact__int_power" [arg1, arg2] = intPower arg1 arg2
decodeFn _ "polypaver__exact__square" [arg1] = square arg1
decodeFn _ "polypaver__exact__sqrt" [arg1] = sqrt arg1
decodeFn _ "polypaver__exact__exp" [arg1] = exp arg1
decodeFn _ "polypaver__exact__sin" [arg1] = sin arg1
decodeFn _ "polypaver__exact__cos" [arg1] = cos arg1
decodeFn original "polypaver__exact__integral" args = decodeIntegral original args

-- We assume abs is exact in FP.  We do not distinguish exact and float variants.
decodeFn _ "abs" [arg1] = abs arg1 

-- functions used in some old SPARK examples, kept for backward compatibility:
decodeFn _ "numeric__divide" [arg1, arg2] = arg1 /: arg2
decodeFn _ "numeric__times" [arg1, arg2] = arg1 *: arg2
decodeFn _ "numeric__plus" [arg1, arg2] = arg1 +: arg2
decodeFn _ "numeric__minus" [arg1, arg2] = arg1 -: arg2
decodeFn _ "num__divide" [arg1, arg2] = arg1 /: arg2
decodeFn _ "num__multiply" [arg1, arg2] = arg1 *: arg2
decodeFn _ "num__add" [arg1, arg2] = arg1 +: arg2
decodeFn _ "num__subtract" [arg1, arg2] = arg1 -: arg2
decodeFn _ "num__square" [arg1] = termOp1 (FSquare 24 126) arg1
decodeFn _ "num__sqrt" [arg1] = termOp1 (FSqrt 24 126) arg1
decodeFn _ "num__exp" [arg1] = termOp1 (FExp 24 126) arg1

decodeFn _ "exact__hull" [arg1, arg2] = hull arg1 arg2
decodeFn _ "exact__interval" [arg1, arg2] = hull arg1 arg2
decodeFn _ "exact__sqrt" [arg1] = sqrt arg1
decodeFn _ "exact__exp" [arg1] = exp arg1
decodeFn _ "exact__sin" [arg1] = sin arg1
decodeFn _ "exact__cos" [arg1] = cos arg1
decodeFn original "exact__integral" args = decodeIntegral original args

decodeFn original fn _args =
    unsafePrint
    (
        "\nWarning: treating the term " ++ show originalNoSpaces ++ " as a variable\n" ++
        "         because the function " ++ show fn ++ " is not recognised by PolyPaver." 
    ) $
    var originalNoSpaces
    where
    originalNoSpaces = filter (not . isSpace) original
--        fn ++ "(" ++ (intercalate "," $ map show args) ++ ")"
--    error $ 
--        "cannot decode function call " ++ fn ++ 
--        "(" ++ (intercalate "," $ map show args) ++ ")"

decodeIntegral :: 
    String -> [Term ()] -> Term ()
decodeIntegral _ [arg1, arg2, arg3] = termOp3 (Integral ivNum ivName) arg1 arg2 arg3
decodeIntegral original [arg1, arg2, arg3, arg4] =
    case arg4 of
        (Term (Var ivNum2 ivName2, _)) -> 
            termOp3 (Integral ivNum2 ivName2) arg1 arg2 arg3
        _ ->
            error $
                "\nInvalid integral: " ++ original ++ 
                "\nThe fourth parameter must be the integration variable."
decodeIntegral original _ =
    error $ "Invalid integral: " ++ original

-- constants used in hand-written VC-style problems:
var :: String -> Term ()
var "FepsAbs" = fepsAbs
var "FepsRel" = fepsRel
var "FepsiAbs" = fepsiAbs
var "FepsiRel" = fepsiRel
var "DepsAbs" = depsAbs
var "DepsRel" = depsRel
var "DepsiAbs" = depsiAbs
var "DepsiRel" = depsiRel
var "Pi" = pi

-- constants declared in the PolyPaver SPARK package and its sub-packages:
var "polypaver__floats__eps_abs" = fepsAbs
var "polypaver__floats__eps_rel" = fepsRel
var "polypaver__floats__plus_minus_eps_abs" = fepsiAbs
var "polypaver__floats__plus_minus_eps_rel" = fepsiRel
var "polypaver__floats__pi" = fround pi
var "polypaver__long_floats__eps_abs" = depsAbs
var "polypaver__long_floats__eps_rel" = depsRel
var "polypaver__long_floats__plus_minus_eps_abs" = depsiAbs
var "polypaver__long_floats__plus_minus_eps_rel" = depsiRel
var "polypaver__long_floats__pi" = dround pi
var "polypaver__exact__pi" = pi
var "polypaver__exact__integration_variable" = termVar ivNum ivName

-- constants used in some old SPARK examples, kept for backward compatibility:
var "numeric__eps_abs" = fepsAbs
var "numeric__eps_rel" = fepsRel
var "num__eps_abs" = fepsAbs
var "num__eps_rel" = fepsRel
var "exact__integration_variable" = termVar ivNum ivName

-- constants of abstract types that the examiner does not understand:
var "universal_real__size" = plusInfinityTerm 
var "universal_real__first" = minusInfinityTerm 
var "universal_real__last" = plusInfinityTerm 
var "universal_real__base__size" = plusInfinityTerm 
var "universal_real__base__first" = minusInfinityTerm 
var "universal_real__base__last" = plusInfinityTerm 

var name =
    termVar n name
    where
    n = sum $ zipWith (*) [1..] $ map ord name

ivNum :: Int
ivNum = -1 
ivName :: String
ivName = "<iv>"

m_integer :: Parser Integer
m_identifier :: Parser String
m_symbol :: String -> Parser String
m_reservedOp :: String -> Parser ()
m_dot :: Parser String
m_reserved :: String -> Parser ()
m_whiteSpace :: Parser ()
m_float :: Parser Double
--m_parens :: Parser a -> Parser a -- MK: this line fails with type a error, no idea why
TokenParser{ parens = m_parens
            , identifier = m_identifier
            , reservedOp = m_reservedOp
            , reserved = m_reserved
            , symbol = m_symbol
            , dot = m_dot
            , integer = m_integer
            , float = m_float
            , whiteSpace = m_whiteSpace } 
            = 
            makeTokenParser tokenDef

tokenDef :: LanguageDef st
tokenDef = emptyDef{ commentStart = "/*"
               , commentEnd = "*/"
               , commentLine = "//"
               , identStart = letter
               , identLetter = alphaNum <|> (oneOf "_")
               , opStart = oneOf $ ">=-+*|&!\\/" ++ "<("
               , opLetter = oneOf $ ">=-+*|&!\\/" ++ ":)"
               , reservedOpNames = 
                    [">", "<", ">=", "<=", "=", "==", "<>", "!=", "/=", 
                     "<-", "|<-|", 
                     "-", "+", "*", "/", "^",
                     "(-)", "(+)", "(*)", "(/)", "-:", "+:", "*:", "/:", 
                     "->", "&&", "&", "||", "|", "/\\", "\\/"]
               , reservedNames = ["and", "or", "implies"]
               }


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
    