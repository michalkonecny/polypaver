{-# LANGUAGE ScopedTypeVariables #-}
{-|
    Module      :  PolyPaver.Input.Haskell
    Description :  loader of problems defined in Haskell modules 
    Copyright   :  (c) Michal Konecny, Jan Duracz, (with code by Jani Hartikainen)
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Dynamic loader of PolyPaver problems defined within Haskell modules.
    Each problem must be defined in a top-level expression in a "Main" module.
    The module cannot depend on anything except the polypaver package and Prelude.
-}

module PolyPaver.Input.Haskell
(
    loadHaskellProblems
)
where

import PolyPaver.Invocation (Problem)

-- the following imports are for the ghc api attempt:
import Control.Exception (throw)
import Control.Exception.Base (ErrorCall(..))
import GHC hiding (loadModule)
import GHC.Paths (libdir)
import HscTypes (SourceError, srcErrorMessages)
import DynFlags
import Unsafe.Coerce
import Bag (bagToList)
import Outputable (runSDoc, defaultErrStyle, initSDocContext, CodeStyle(..))

-- the following imports are for the plugins attempt:
--import Text.Regex.Posix
--import System.Eval.Haskell

loadHaskellProblems ::
    FilePath ->
    [String] ->
    IO [Problem]
loadHaskellProblems 
    moduleFilePath
    problemNames
    =
    runGhc (Just libdir) $
        do
        loadrst <- loadSourceGhc moduleFilePath
        case loadrst of
            Just err -> error err
            Nothing ->
                do
                mapM (execFnGhc moduleName) problemNames
    where
    moduleName = "Main"
    
         
--- The following is an adaptation of code by Jani Hartikainen at:
--  http://codeutopia.net/blog/2011/08/20/adventures-in-haskell-dynamic-loading-and-compiling-of-modules

loadSourceGhc :: String -> Ghc (Maybe String)
loadSourceGhc path = 
    do
    dflagsPre <- getSessionDynFlags
    let dflags = adaptDFlags dflagsPre
    setSessionDynFlags dflags
    target <- guessTarget path Nothing
    addTarget target
    r <- load LoadAllTargets
    case r of
        Failed    -> return $ Just "Generic module load error"
        Succeeded -> return Nothing
 
    `gcatch` \(e :: SourceError) -> let
            errors e = concat $ map show (bagToList $ srcErrorMessages e)
        in
            return $ Just (errors e)
    where
    adaptDFlags dflagsPre =
        dflagsPre 
        {
            ghcLink = LinkInMemory,
            hscTarget = HscInterpreted,
--            packageFlags = [ExposePackage "polypaver"],
            log_action = throwingLogAction
        }
    throwingLogAction dflags SevError _ _ msg = throw $ ErrorCall $ showM dflags msg
    throwingLogAction dflags SevFatal _ _ msg = throw $ ErrorCall $ showM dflags msg
    throwingLogAction dflags _ _ _ _ = return ()
    showM dflags msg = show $ runSDoc msg (initSDocContext dflags (defaultErrStyle dflags))
                
execFnGhc :: String -> String -> Ghc a
execFnGhc modname fn = 
    do
    let moduleName = mkModuleName modname
--    mod <- findModule moduleName Nothing
    setContext [IIModule moduleName]
    value <- compileExpr (modname ++ "." ++ fn)
    let value' = (unsafeCoerce value) :: a
    return value'

    
---- version based on plugins:
---- when tried on 8th May 2013 with ghc 7.4.2 and plugins 1.5.3.0, it failed with a linker error
--    
--loadHaskellProblems ::
--    FilePath ->
--    [String] ->
--    IO [Problem]
--loadHaskellProblems 
--    moduleFilePath
--    problemNames
--    =
--    do
--    moduleContent <- readFile moduleFilePath
--    let problemExpressions = map (locateExpression moduleFilePath moduleContent) problemNames
--    mapM evalProblem problemExpressions 
--    
--    
--locateExpression :: FilePath -> String -> String -> String
--locateExpression moduleFilePath moduleContent expressionName =
--    unlines $ isolateExpressionLines $ lines moduleContent
--    where
--    isolateExpressionLines [] = errorNotFound
--    isolateExpressionLines (line : lines)
--        | line =~ startRegexp = 
--            case line =~ startRegexp of
--                (_before :: String, _match :: String, after)
--                    | after =~ "^\\s*$" -> findEnd lines
--                    | otherwise -> findEnd $ (" " ++ after) : lines 
--                     
--        | otherwise = 
--            isolateExpressionLines lines
--        where
--        startRegexp = expressionName ++ "\\s*=\\s*"
--    findEnd [] = []
--    findEnd (line : lines) 
--        | line =~ "^\\s" = line : (findEnd lines)
--        | otherwise = []
--        
--    errorNotFound = error $ "The problem " ++ show expressionName ++ " not found in file " ++ moduleFilePath
--
--evalProblem :: String -> IO Problem
--evalProblem expression =
--    do
--    errorsOrMaybeValue <- eval_ expression ["PolyPaver"] ["-w"] [] []
--    case errorsOrMaybeValue of
--        Left errors ->
--            do
--            putStrLn $ "Left" 
--            error $ unlines errors
--        Right (Just problem) -> return problem
--        _ -> error $ "unknown problem when interpreting the expression:\n" ++ expression
    