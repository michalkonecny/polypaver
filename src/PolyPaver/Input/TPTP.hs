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
import PolyPaver.Vars
import PolyPaver.DeriveBounds

import qualified Codec.TPTP as TPTP (parse)

--import Data.Char (ord)
--import Data.List (intercalate, partition)
--import qualified Data.IntMap as IMap

parseTPTP ::
    String {-^ description of the source (eg file name) for error reporting -} ->
    String {-^ the contents of the TPTP file -} -> 
    (String, Form (), [(Int, (Rational, Rational), Bool)])
    {-^ the formula and the bounding box for its variables -}
parseTPTP _sourceDescription s =
    case TPTP.parse s of
        items -> 
            error $ "TPTP: found " ++ (show $ length items) ++ " items"
--            addBox (goalName, formula )
--            where
--            (formula, goalName) = tptp_formula_2_PP_formula items

--tptp_file =
--    many tptp_input
--    
--tptp_input =
--    annotated_formula <|> include
--    
--annotated_formula =
--    thf_annotated <|> tff_annotated <|> fof_annotated <|> cnf_annotated <|> tpi_annotated
--    
--tpi_annotated = formula_annotated "tpi" fof_formula
--thf_annotated = formula_annotated "thf" $ parserFail "thf not supported"
--tff_annotated = formula_annotated "tff" $ parserFail "tff not supported"
--fof_annotated = formula_annotated "fof" fof_formula
--cnf_annotated = formula_annotated "cnf" cnf_formula
--
--formula_annotated flavour formulaParser =
--    do
--    optional m_whiteSpace
--    string flavour
--    string "("
--    name <- m_symbol
--    m_comma
--    role <- formula_role
--    form <- formulaParser
--    annotations
--    m_symbol ")"
--    return (name, role, form)
--    
--formula_role =
--    choice $ map try $ map string $ 
--        ["axiom", "hypothesis", "definition", "assumption",
--         "lemma", "theorem", "conjecture", "negated_conjecture",
--         "plain", "fi_domain", "fi_functors", "fi_predicates",
--         "type", "unknown"]
--
--fof_formula =
--    (try fof_logic_formula) <|> fof_sequent
--    
--fof_logic_formula =
--    (try fof_binary_formula) <|> fof_unitary_formula
--
--fof_binary_formula =
--    (try fof_binary_nonassoc) <|> fof_binary_assoc
--    
--fof_binary_nonassoc =
--    do
--    form1 <- fof_unitary_formula
--    binOp <- binary_connective
--    form2 <- fof_unitary_formula
--    return $ form1 `binOp` form2
--
--fof_binary_assoc =
--    (try fof_or_formula) <|> fof_and_formula
--    
--fof_or_formula = fof_binary_assoc_op "|" Or
--fof_and_formula = fof_binary_assoc_op "&" And
--    
--fof_binary_assoc_op opSymbol binOp =
--    (try option1) <|> option2
--    where
--    option1 =
--        do
--        f1 <- fof_unitary_formula
--        m_symbol opSymbol
--        f2 <- fof_unitary_formula
--        return $ f1 `binOp` f2
--    option2 =
--        do
--        f1 <- fof_or_formula
--        m_symbol "|"
--        f2 <- fof_unitary_formula
--        return $ f1 `binOp` f2
--
--fof_unitary_formula =
--    fof_quantified_formula 
--    <|> try fof_unary_formula
--    <|> try atomic_formula
--    <|> fof_logic_formula
--    
--fof_quantified_formula =
--    do
--    q <- fol_quantifier
--    vars <- optional fof_variable_list
--    m_reservedOp ":"
--    f <- fof_unitary_formula
--    return $ Forall vars f
--    
--fof_variable_list =
--    do
--    v1 <- variable
--    maybeVRest <- optionMaybe $ fof_variable_list
--    case maybeVRest of
--        Just vRest -> return $ v1 : vRest
--        Nothing -> return [v1] 
--        
--fof_unary_formula =
--    (try $ option1) <|> option2
--    where
--    option1 =
--        do
--        upOp <- unary_connective
--        f <- fof_unitary_formula
--        return $ upOp f
--    option2 = fol_infix_unary
--    
--    
--m_stringLiteral_sq =
--    do
--    oneOf "'"
--    content <- many $ try $ (string "''" <|> (anyCharButSingleQuote))
--    m_symbol "'"
--    return $ "'" ++ content ++ "'"
--    where
--    anyCharButSingleQuote =
--        do
--        t <- anyChar
--        case t of
--            '\'' -> parserFail "quote inside string"
--            _ -> return [t]
--
--TokenParser{ parens = m_parens
--            , stringLiteral = m_stringLiteral_do
--            , identifier = m_identifier
--            , reservedOp = m_reservedOp
--            , reserved = m_reserved
--            , symbol = m_symbol
--            , dot = m_dot
--            , comma = m_comma
--            , integer = m_integer
--            , whiteSpace = m_whiteSpace } 
--            = 
--            makeTokenParser tokenDef
--
--tokenDef = emptyDef{ commentStart = "/*"
--               , commentEnd = "*/"
--               , commentLine = "%"
--               , identStart = letter
--               , identLetter = alphaNum <|> (oneOf "_")
--               , opStart = oneOf "><=-+*/"
--               , opLetter = oneOf "=>"
--               , reservedOpNames = [">", "<", ">=", "<=", "=", "<>", "-", "+", "*", "/", "->"]
--               , reservedNames = ["and", "or", "implies"]
--               }

     