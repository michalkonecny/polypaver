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
import qualified Data.List as List
import qualified Data.IntMap as IMap

import Data.Hashable

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
    (Eq l, HasDefaultValue l, Hashable l) =>
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

decodePred, decodePredAux :: FormLabel -> String -> [Term l] -> Form l
decodePred lab name =
    decodePredAux lab (removeFPTypeCode name)

decodePredAux lab "pp_integer__is_integer" [arg1] = IsInt lab arg1
decodePredAux lab "pp_integer__is_range" [arg1, arg2, arg3] = IsIntRange lab arg1 arg2 arg3
decodePredAux lab "pp_exact__is_range" [arg1, arg2, arg3] = IsRange lab arg1 arg2 arg3
decodePredAux lab "pp_exact__contained_in" [arg1, arg2] = ContainedIn lab arg1 arg2

decodePredAux lab "num__isint" [arg1] = IsInt lab arg1
decodePredAux lab "num__isintegerrange" [arg1, arg2, arg3] = IsIntRange lab arg1 arg2 arg3
decodePredAux lab "num__isfloatrange" [arg1, arg2, arg3] = IsRange lab arg1 arg2 arg3
decodePredAux lab "num__isdoublerange" [arg1, arg2, arg3] = IsRange lab arg1 arg2 arg3
decodePredAux lab "exact__containedin" [arg1, arg2] = ContainedIn lab arg1 arg2

decodePredAux lab "integer" [arg1] = IsInt lab arg1

decodePredAux lab pred2 args =
    error $ 
        "in [" ++ lab ++ "], cannot decode predicate " ++ pred2 ++ 
        "(" ++ (intercalate "," $ map show args) ++ ")"


term :: Parser (Term ())
term = buildExpressionParser termTable atomicTerm <?> "term"

termTable :: [[Operator String () Identity (Term ())]]
termTable = 
    [ [prefix "-" negate]
    , [binary "^" (termOp2 IntPower) AssocLeft]
    , [binary "**" (termOp2 IntPower) AssocLeft]
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
decodeFn, decodeFnAux :: 
    String -> String -> [Term ()] -> Term ()
decodeFn original fnname args = 
    decodeFnAux original (removeFPTypeCode fnname) args

removeFPTypeCode :: String -> String
removeFPTypeCode name 
    | "pp_f_" `List.isPrefixOf` name = "pp_" ++ (drop 5 name)
    | "pp_lf_" `List.isPrefixOf` name = "pp_" ++ (drop 6 name)
    | "pp_sf_" `List.isPrefixOf` name = "pp_" ++ (drop 6 name)
    | otherwise = name

decodeFnAux _ "Hull" [arg1, arg2] = hull arg1 arg2
decodeFnAux _ "Interval" [arg1, arg2] = hull arg1 arg2
decodeFnAux _ "Sqrt" [arg1] = sqrt arg1
decodeFnAux _ "Exp" [arg1] = exp arg1
--decodeFnAux _ "Sin" [arg1] = sin arg1
--decodeFnAux _ "Cos" [arg1] = cos arg1
decodeFnAux original "Integral" args = decodeIntegral original args

-- functions generated by the pp_fpops pre-processor for FP operators and elementary functions:
decodeFnAux _ "pp_rounded__divide" [precArg, arg1, arg2] = withPrec precArg termOp2 FOver arg1 arg2
decodeFnAux _ "pp_rounded__multiply" [precArg, arg1, arg2] = withPrec precArg termOp2 FTimes arg1 arg2
decodeFnAux _ "pp_rounded__plus" [precArg, arg1, arg2] = withPrec precArg termOp2 FPlus arg1 arg2
decodeFnAux _ "pp_rounded__minus" [precArg, arg1, arg2] = withPrec precArg termOp2 FMinus arg1 arg2
decodeFnAux _ "pp_rounded__square" [precArg, arg1] = withPrec precArg termOp1 FSquare arg1
decodeFnAux _ "pp_rounded__sqrt" [precArg, arg1] = withPrec precArg termOp1 FSqrt arg1
--decodeFnAux _ "pp_rounded__sin" [precArg, arg1] = termOp1 (FSin 24 126) arg1
--decodeFnAux _ "pp_rounded__cos" [precArg, arg1] = termOp1 (FCos 24 126) arg1
decodeFnAux _ "pp_rounded__exp" [precArg, arg1] = withPrec precArg termOp1 FExp arg1

-- functions with exact semantics, from the PP_F_Exact, etc packages: 
decodeFnAux _ "pp_exact__eps_abs" [precArg] = withPrec precArg termOp0 FEpsAbs
decodeFnAux _ "pp_exact__eps_rel" [precArg] = withPrec precArg termOp0 FEpsRel
decodeFnAux _ "pp_exact__plus_minus_eps_abs" [precArg] = withPrec precArg termOp0 FEpsiAbs
decodeFnAux _ "pp_exact__plus_minus_eps_rel" [precArg] = withPrec precArg termOp0 FEpsiRel
decodeFnAux _ "pp_exact__interval" [arg1, arg2] = hull arg1 arg2
decodeFnAux _ "pp_exact__exponentiate" [arg1, arg2] = intPower arg1 arg2
decodeFnAux _ "pp_exact__square" [arg1] = square arg1
decodeFnAux _ "pp_exact__sqrt" [arg1] = sqrt arg1
decodeFnAux _ "pp_exact__exp" [arg1] = exp arg1
--decodeFnAux _ "pp_exact__sin" [arg1] = sin arg1
--decodeFnAux _ "pp_exact__cos" [arg1] = cos arg1
decodeFnAux original "pp_exact__integral" args = decodeIntegral original args

-- We assume abs is exact in FP.  We do not distinguish exact and float variants.
decodeFnAux _ "abs" [arg1] = abs arg1 

-- functions used in some old SPARK examples, kept for backward compatibility:
decodeFnAux _ "numeric__divide" [arg1, arg2] = arg1 /: arg2
decodeFnAux _ "numeric__times" [arg1, arg2] = arg1 *: arg2
decodeFnAux _ "numeric__plus" [arg1, arg2] = arg1 +: arg2
decodeFnAux _ "numeric__minus" [arg1, arg2] = arg1 -: arg2
decodeFnAux _ "num__divide" [arg1, arg2] = arg1 /: arg2
decodeFnAux _ "num__multiply" [arg1, arg2] = arg1 *: arg2
decodeFnAux _ "num__add" [arg1, arg2] = arg1 +: arg2
decodeFnAux _ "num__subtract" [arg1, arg2] = arg1 -: arg2
decodeFnAux _ "num__square" [arg1] = termOp1 (FSquare 24 126) arg1
decodeFnAux _ "num__sqrt" [arg1] = termOp1 (FSqrt 24 126) arg1
decodeFnAux _ "num__exp" [arg1] = termOp1 (FExp 24 126) arg1

decodeFnAux _ "exact__hull" [arg1, arg2] = hull arg1 arg2
decodeFnAux _ "exact__interval" [arg1, arg2] = hull arg1 arg2
decodeFnAux _ "exact__sqrt" [arg1] = sqrt arg1
decodeFnAux _ "exact__exp" [arg1] = exp arg1
--decodeFnAux _ "exact__sin" [arg1] = sin arg1
--decodeFnAux _ "exact__cos" [arg1] = cos arg1
decodeFnAux original "exact__integral" args = decodeIntegral original args

decodeFnAux original fn _args =
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

withPrec ::
    Term l -> (op -> mkTerm) -> (Int -> Int -> op) -> mkTerm
withPrec precArg termBuilder op =
    termBuilder (op relB absB)
    where
    (Term (Lit adaDigitsRational, _)) = precArg
    relB
        | adaDigitsRational == 6 = 23
        | adaDigitsRational == 15 = 53
        | otherwise =
            floor $ 3.321928094887362 * (adaDigitsDbl + 1)
    absB 
        | adaDigitsRational >= 15 = 1022
        | adaDigitsRational >= 6 = 126
        | otherwise = relB
    adaDigitsDbl = fromRational adaDigitsRational :: Double

-- constants used in hand-written VC-style problems:
var, varAux :: String -> Term ()
var name = varAux (removeFPTypeCode name)

varAux "FepsAbs" = fepsAbs
varAux "FepsRel" = fepsRel
varAux "FepsiAbs" = fepsiAbs
varAux "FepsiRel" = fepsiRel
varAux "DepsAbs" = depsAbs
varAux "DepsRel" = depsRel
varAux "DepsiAbs" = depsiAbs
varAux "DepsiRel" = depsiRel
varAux "Pi" = pi

-- constants declared in the PP_F_Exact, et al packages:
varAux "pp_exact__pi" = pi
varAux "pp_exact__integration_variable" = termVar ivNum ivName

-- constants used in some old SPARK examples, kept for backward compatibility:
varAux "numeric__eps_abs" = fepsAbs
varAux "numeric__eps_rel" = fepsRel
varAux "num__eps_abs" = fepsAbs
varAux "num__eps_rel" = fepsRel
varAux "exact__integration_variable" = termVar ivNum ivName

-- constants of abstract types that the examiner does not understand:
varAux "universal_real__size" = plusInfinityTerm 
varAux "universal_real__first" = minusInfinityTerm 
varAux "universal_real__last" = plusInfinityTerm 
varAux "universal_real__base__size" = plusInfinityTerm 
varAux "universal_real__base__first" = minusInfinityTerm 
varAux "universal_real__base__last" = plusInfinityTerm 

varAux name =
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
    