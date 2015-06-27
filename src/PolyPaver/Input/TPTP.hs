{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-|
    Module      :  PolyPaver.Input.SPARK
    Description :  parser of SPARK vcg and siv files 
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Parser of TPTP files as PolyPaver problems.
    
    Based on http://www.cs.miami.edu/~tptp/TPTP/SyntaxBNF.html. 
-}

module PolyPaver.Input.TPTP 
(
    parseTPTP
)
where

import PolyPaver.Form

import PolyPaver.Input.Misc (withConsumed, removeDisjointHypothesesAddBox, traceRuleDoIt)

import Data.Char (ord, isUpper, isLower, isSpace)
import qualified Data.List as List (partition)

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Token
import Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))
--import Text.Parsec.Prim
import Text.Parsec.Language (emptyDef)

import Data.Functor.Identity (Identity)
import Data.Maybe (catMaybes)

import Debug.Trace (trace)
_ = trace -- prevent unused import warning
_ = traceRuleDoIt

traceRule :: String -> Parser a -> Parser a
traceRule _ = id
--traceRule = traceRuleDoIt

parseTPTP ::
    String {-^ description of the source (eg file name) for error reporting -} ->
    String {-^ the contents of the TPTP file -} -> 
    [(String, Form (), [(Int, (Rational, Rational), Bool)])]
    {-^ the formula and the bounding box for its variables -}
parseTPTP sourceDescription s =
    case parse tptp_file sourceDescription s of
        Right formulas -> map removeDisjointHypothesesAddBox $ mergeHypothesesIntoConjectures $ map insertNameToForm formulas
        Left err -> error $ "parse error in " ++ show err
        where
        insertNameToForm (name, formrole, form) =
            (name, formrole, insertLabel name form)
        mergeHypothesesIntoConjectures formulas =
            map addHypotheses conjecturesNRF
            where
            addHypotheses (name, "negated_conjecture", anticonj) = 
                (name, joinHypothesesAndConclusion (hypotheses, Not anticonj))
            addHypotheses (name, _formrole, conj) = 
                (name, joinHypothesesAndConclusion (hypotheses, conj))
            hypotheses = map dropNameRole hypothesesNRF
                where
                dropNameRole (_name , _formrole , form) = form
            (hypothesesNRF, conjecturesNRF) = List.partition canAssume formulas
                where
                canAssume (_name, formrole, _form) =
                    formrole `elem` 
                        ["axiom", "hypothesis", "definition", "assumption",
                         "fi_domain", "fi_functors", "fi_predicates", "type"]
         
    
tptp_file :: Parser [(String, String, Form ())]
tptp_file =
    traceRule ("tptp_file") $
    do
    maybeFormulas <- many1 tptp_input
    optional m_whiteSpace
    eof
    return $ catMaybes maybeFormulas
    
tptp_input :: Parser (Maybe (String, String, Form ()))
tptp_input =
    traceRule ("tptp_input") $
    do
    optional m_whiteSpace
    (include <|> (fmap Just annotated_formula)) 
    
include :: Parser (Maybe a)
include =
    traceRule ("include") $
    do
    (_, original) <- withConsumed includeAux
    trace ("Warning: ignoring " ++ original) $ return Nothing
    where
    includeAux =
        do
        string "include"
        m_parens term
        m_dot
        return Nothing
    
annotated_formula :: Parser (String, String, Form ())
annotated_formula =
    traceRule ("annotated_formula") $
    ((try thf_annotated) <|> (try tff_annotated) <|> fof_annotated <|> cnf_annotated <|> tpi_annotated)

tpi_annotated, thf_annotated, tff_annotated, fof_annotated, cnf_annotated :: Parser (String, String, Form ())
    
tpi_annotated = formula_annotated "tpi" fof_formula_allow_quant
thf_annotated = formula_annotated "thf" $ parserFail "thf not supported"
tff_annotated = formula_annotated "tff" $ parserFail "tff not supported"
fof_annotated = formula_annotated "fof" fof_formula_allow_quant
cnf_annotated = formula_annotated "cnf" $ parserFail "cnf not supported"

formula_annotated :: 
    String -> 
    Parser (Form ()) -> 
    Parser (String, String, Form ())
formula_annotated flavour formulaParser =
    traceRule ("formula_annotated " ++ flavour) $
    do
    optional m_whiteSpace
    string flavour
    string "("
    name <- m_identifier
    m_comma
--    trace ("name = " ++ name) $ return ()
    formrole <- formula_role
    m_comma
--    trace ("formrole = " ++ formrole) $ return ()
    form <- formulaParser
    optional annotations
    m_symbol ")"
    m_dot
    return (name, formrole, form)
    
annotations :: Parser a
annotations =
    traceRule ("annotations") $
    do
    m_comma
    parserFail "annotations not supported"
    
formula_role :: Parser String
formula_role =
    choice $ map try $ map string $ 
        ["axiom", "hypothesis", "definition", "assumption",
         "lemma", "theorem", "conjecture", "negated_conjecture",
         "plain", "fi_domain", "fi_functors", "fi_predicates",
         "type", "unknown"]

fof_formula_allow_quant :: Parser (Form ())
fof_formula_allow_quant =
    traceRule ("fof_formula_allow_quant") $
    (try fof_logic_formula_allow_quant <|> fof_sequent)
    
fof_sequent :: Parser (Form ())
fof_sequent =
    traceRule ("fof_sequent") $
    do
    string "["
    parserFail "fof_sequent not supported"
    
fof_logic_formula_allow_quant :: Parser (Form ())
fof_logic_formula_allow_quant =
    traceRule ("fof_logic_formula_allow_quant") $
    (try fof_binary_formula <|> fof_unitary_formula_allow_quant)

fof_logic_formula :: Parser (Form ())
fof_logic_formula =
    traceRule ("fof_logic_formula") $
    (try fof_binary_formula <|> fof_unitary_formula)

fof_binary_formula :: Parser (Form ())
fof_binary_formula =
    traceRule ("fof_binary_formula") $
    (try fof_binary_nonassoc <|> fof_binary_assoc)
    
fof_binary_nonassoc :: Parser (Form ())
fof_binary_nonassoc =
    traceRule ("fof_binary_nonassoc") $
    do
    form1 <- fof_unitary_formula
    optional m_whiteSpace
    binOp <- binary_connective
    optional m_whiteSpace
    form2 <- fof_unitary_formula
    return $ form1 `binOp` form2

binary_connective :: Parser (Form l -> Form l -> Form l)
binary_connective =
    traceRule ("binary_connective") $
    (foldr (<|>) (parserFail "Expecting a TPTP binary logical connective.") $ 
        map connective_parser connectives)
    where
    connectives =
        [("<=>", Nothing), 
         ("=>", Just Implies),
--         ("<=", Just (flip Implies)), -- removed to avoid clash with leq 
         ("<~>", Nothing), 
         ("~|", Nothing),
         ("~&", Nothing)]
    connective_parser (symb, Nothing) = 
        do 
        try $ m_reservedOp symb
        parserFail (symb ++ " not supported")
    connective_parser (symb, Just makeFormula) =
        do 
        m_reservedOp symb
        return makeFormula

unary_connective :: Parser (Form l -> Form l)
unary_connective =
    traceRule ("unary_connective") $
    (foldr (<|>) (parserFail "Expecting a TPTP unary logical connective.") $ 
        map connective_parser connectives)
    where
    connectives =
        [("~", Just Not)]
    connective_parser (symb, Nothing) = 
        do 
        try $ m_reservedOp symb
        parserFail (symb ++ " not supported")
    connective_parser (symb, Just makeFormula) =
        do 
        m_reservedOp symb
        return makeFormula

fof_binary_assoc :: Parser (Form ())
fof_binary_assoc =
    traceRule ("fof_binary_assoc") $
    (try fof_or_formula) <|> fof_and_formula
    
fof_or_formula :: Parser (Form ())
fof_or_formula = 
    traceRule ("fof_or_formula") $
    fof_binary_assoc_op "|" Or
fof_and_formula :: Parser (Form ())
fof_and_formula = 
    traceRule ("fof_and_formula") $
    fof_binary_assoc_op "&" And
    
fof_binary_assoc_op :: 
    String -> 
    (Form () -> Form () -> Form ()) -> 
    Parser (Form ())
fof_binary_assoc_op opSymbol binOp =
    traceRule ("fof_binary_assoc_op") $
    (try option1) <|> option2
    where
    option1 =
        do
        f1 <- fof_unitary_formula
        m_symbol opSymbol
        f2 <- fof_unitary_formula
        return $ f1 `binOp` f2
    option2 =
        do
        f1 <- fof_unitary_formula -- swapped L<->R in this TPTP rule to remove left recursion
        m_symbol opSymbol
        f2 <- fof_binary_assoc_op opSymbol binOp
        return $ f1 `binOp` f2

fof_unitary_formula_allow_quant :: Parser (Form ())
fof_unitary_formula_allow_quant =
    traceRule ("fof_unitary_formula_allow_quant") $
    (fof_quantified_formula
    <|> fof_quantified_formula_error "?"
    <|> try (fof_unary_formula)
    <|> try (atomic_formula)
    <|> m_parens fof_logic_formula)
    
fof_unitary_formula :: Parser (Form ())
fof_unitary_formula =
    traceRule ("fof_unitary_formula") $
    (fof_quantified_formula_error "!"
    <|> fof_quantified_formula_error "?"
    <|> try (fof_unary_formula)
    <|> try (atomic_formula)
    <|> m_parens fof_logic_formula)
    
fof_quantified_formula_error :: String -> Parser (Form ())
fof_quantified_formula_error quantifier =
    traceRule ("fof_quantified_formula_error " ++ quantifier) $
    do
    m_reservedOp quantifier
    parserFail $ "quantified formulas not supported, except top level `for all'"
    undefined
    
fof_quantified_formula :: Parser (Form ())
fof_quantified_formula =
    traceRule ("fof_quantified_formula") $
    do
    m_reservedOp "!"
--    trace ("fof_quantified_formula: `!'") $ return ()
    _vars <- m_brackets fof_variable_list
    optional m_whiteSpace
    m_reservedOp ":"
--    trace ("fof_quantified_formula: `:'") $ return ()
    optional m_whiteSpace
    f <- fof_unitary_formula_allow_quant
    return $ f
    
fof_variable_list :: Parser [String] 
fof_variable_list =
    traceRule ("fof_variable_list") $
    do
    optional m_whiteSpace
    v1 <- m_UpperWord -- originally: variable
    optional m_whiteSpace
    maybeVRest <- optionMaybe $ (m_comma >> fof_variable_list)
    case maybeVRest of
        Just vRest -> return $ v1 : vRest
        Nothing -> return [v1]

fof_unary_formula :: Parser (Form ())
fof_unary_formula =
    traceRule ("fof_unary_formula") $
    (try option1) <|> option2
    where
    option1 =
        do
        upOp <- unary_connective
        f <- fof_unitary_formula
        return $ upOp f
    option2 = fol_infix_unary

fol_infix_unary :: Parser (Form ()) 
fol_infix_unary =
    traceRule ("fol_infix_unary") $
    do
    tL <- term
--    trace ("fol_infix_unary: got term: " ++ show tL) $ return ()
    optional m_whiteSpace
    m_reservedOp "!="
    optional m_whiteSpace
    tR <- term 
    return $ Neq "" tL tR

atomic_formula :: Parser (Form ()) 
atomic_formula =
    traceRule ("atomic_formula") $
    (try defined_atomic_formula) <|> (try system_term) <|> plain_term  

defined_atomic_formula :: Parser (Form ()) 
defined_atomic_formula =
    traceRule ("defined_atomic_formula") $
    (try defined_plain_term) <|> defined_infix_formula

system_term :: Parser (Form ())
system_term =
    traceRule ("system_term") $
    do
    string "$$"
    parserFail "system_term ($$...) not supported"

defined_plain_term :: Parser (Form ()) 
defined_plain_term =
    traceRule ("defined_plain_term") $
    do
    string "$"
    parserFail "defined_plain_term ($...) not supported"

defined_infix_formula :: Parser (Form ()) 
defined_infix_formula =
    traceRule ("defined_infix_formula") $
    do
    tL <- term
--    trace ("defined_infix_formula: got term " ++ show tL) $ return ()
    optional m_whiteSpace
    relOp <- binary_relation
--    trace "defined_infix_formula: `='" $ return ()
    optional m_whiteSpace
    tR <- term 
    return $ relOp tL tR

binary_relation :: Parser (Term l -> Term l -> Form l)
binary_relation =
    traceRule ("binary_relation") $
    foldr (<|>) (parserFail "Expecting a TPTP binary relation.") $ 
        map relation_parser relations
    where
    relations =
        [("=", Eq ""), 
         ("<", Le ""),
         ("<=", Leq ""),
         (">", Ge ""),
         (">=", Geq "")
        ]
    relation_parser (symb, makeFormula) =
        do 
        m_reservedOp symb
        return makeFormula


plain_term :: Parser (Form ()) 
plain_term = -- deviating from TPTP in a manner similar to Metitarski 
    predicate

predicate :: Parser (Form ())
predicate =
    traceRule ("predicate") $
    do
    pname <- m_AtomicWord
    maybeArgs <- optionMaybe $ m_parens $ sepBy term (m_symbol ",")
    let args = case maybeArgs of Just args1 -> args1; Nothing -> []
    return $ decodePred pname args

decodePred :: String -> [Term ()] -> Form ()
decodePred pname _args =
--    error $ "decodePred: unknown predicate " ++ pname
    IsInt "" (var pname) -- TODO: change IsInt -> IsTrue

term :: Parser (Term ())
term = -- deviating from TPTP in a manner similar to Metitarski
    buildExpressionParser termTable atomicTerm <?> "term"

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
atomicTerm = 
    traceRule ("atomicTerm") $
    m_parens term
    <|> absBrackets term
    <|> try fncall
    <|> (try $ fmap (fromRational . toRational) m_float)
    <|> fmap (fromInteger) m_integer
    <|> (try $ fmap var m_UpperWord)
    <|> (try $ fmap constant m_AtomicWord)
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
        fname <- m_AtomicWord
        args <- m_parens $ sepBy term (m_symbol ",")
        return (fname, args)
    return $ decodeFn original fname args

var :: String -> Term ()
var name =
    termVar n name
    where
    n = sum $ zipWith (*) [1..] $ map ord name

constant :: String -> Term ()
constant name =
--    trace
--    (
--        "\nWarning: treating the term " ++ show name ++ " as a variable\n" ++
--        "         because the constant " ++ show name ++ " is not recognised by PolyPaver." 
--    ) $
    var name


decodeFn :: String -> String -> [Term ()] -> Term ()
decodeFn _ "exp" [arg1] = exp arg1
decodeFn _ "sqrt" [arg1] = sqrt arg1
decodeFn _ "eps_rel" [mantissaBitsT, expBitsT] =
    withPrec (floor mantissaBits) (floor expBits) termOp0 FEpsRel
    where
    (Term (Lit mantissaBits, _)) = mantissaBitsT
    (Term (Lit expBits, _)) = expBitsT
decodeFn _ "eps_abs" [mantissaBitsT, expBitsT] =
    withPrec (floor mantissaBits) (floor expBits) termOp0 FEpsAbs
    where
    (Term (Lit mantissaBits, _)) = mantissaBitsT
    (Term (Lit expBits, _)) = expBitsT
decodeFn _ "round" [mantissaBitsT, expBitsT, _roundingModeT, arg] = 
    withPrec (floor mantissaBits) (floor expBits) termOp1 FRound arg
    where
    (Term (Lit mantissaBits, _)) = mantissaBitsT
    (Term (Lit expBits, _)) = expBitsT
decodeFn _ "eps_rel_single" [] =
    withPrec 24 8 termOp0 FEpsRel
decodeFn _ "eps_abs_single" [] =
    withPrec 24 8 termOp0 FEpsAbs
decodeFn _ "round_single" [_roundingModeT, arg] = 
    withPrec 24 8 termOp1 FRound arg
decodeFn _ "eps_rel_double" [] =
    withPrec 53 11 termOp0 FEpsRel
decodeFn _ "eps_abs_double" [] =
    withPrec 53 11 termOp0 FEpsAbs
decodeFn _ "round_double" [_roundingModeT, arg] = 
    withPrec 53 11 termOp1 FRound arg
decodeFn original fn _args =
    trace
    (
        "\nWarning: treating the term " ++ show originalNoSpaces ++ " as a variable\n" ++
        "         because the function " ++ show fn ++ " is not recognised by PolyPaver." 
    ) $
    var originalNoSpaces
    where
    originalNoSpaces = filter (not . isSpace) original

withPrec :: 
    Int -> Int -> (s -> t) -> (Int -> Int -> s) -> t
withPrec mantissaBits expBits termBuilder op =
    (termBuilder $ op epsrelE epsabsE)
    where
    epsrelE = mantissaBits - 1 -- TODO sharpen this if rounding to nearest
    epsabsE = 2^(expBits - 1) - 2

m_UpperWord :: Parser String
m_UpperWord =
    do
    name <- m_identifier
    if isUpper (head name) 
        then return name
        else parserFail "Expecting an identifier starting with a upper-case letter." 
    
m_LowerWord :: Parser String
m_LowerWord =
    do
    name <- m_identifier
    if isLower (head name) 
        then return name
        else parserFail "Expecting an identifier starting with a lower-case letter." 
    
m_AtomicWord :: Parser String
m_AtomicWord =
    m_LowerWord <|> single_quoted
    where
    single_quoted =
        do 
        string "'"
        name <- many $ noneOf "'\n\r"
        string "'"
        return name
    
m_integer :: Parser Integer
m_identifier :: Parser String
m_symbol :: String -> Parser String
m_reservedOp :: String -> Parser ()
m_comma :: Parser String
m_dot :: Parser String
--m_reserved :: String -> Parser ()
m_whiteSpace :: Parser ()
m_float :: Parser Double
m_parens :: ParsecT String t Identity a -> ParsecT String t Identity a
m_brackets :: ParsecT String t Identity a -> ParsecT String t Identity a
TokenParser{ parens = m_parens
            , brackets = m_brackets
--            , stringLiteral = m_stringLiteral_do
            , identifier = m_identifier
            , reservedOp = m_reservedOp
--            , reserved = m_reserved
            , symbol = m_symbol
            , dot = m_dot
            , comma = m_comma
            , integer = m_integer
            , float = m_float
            , whiteSpace = m_whiteSpace } 
            = 
            makeTokenParser tokenDef

tokenDef :: LanguageDef st
tokenDef = emptyDef{ commentStart = "/*"
               , commentEnd = "*/"
               , commentLine = "%"
               , identStart = letter
               , identLetter = alphaNum <|> (oneOf "_")
               , opStart = oneOf "><=-+*/!?:&|~"
               , opLetter = oneOf "=>~|&"
               , reservedOpNames = 
                    [">", "<", ">=", "<=", "=", "!=", 
                     "-", "+", "*", "/", 
                     ":", "!", "?", 
                     "~", "&", "|", "<=>", "=>", "<=", "<~>", "~|", "~&"]
               }

