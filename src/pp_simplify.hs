{-|
    Module      :  Main
    Description :  the pp_simplify executable  
    Copyright   :  (c) Michal Konecny 
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    The pp_simplify executable for simplifying PolyPaver problems.
-}
module Main where

import PolyPaver.Simplify.Args
import PolyPaver.Simplify.Substitution

import PolyPaver.Subterms (TermHash, addHashesInForm)
import PolyPaver.Invocation (Problem(..), reportCmdLine)
import PolyPaver.Form (Form, splitConclusion)
import PolyPaver.Input.SPARK
import PolyPaver.DeriveBounds (getBox)

import System.Console.CmdArgs (cmdArgs)

--import Numeric.ER.Real.DefaultRepr

--import qualified Data.IntMap as IMap
import Data.List

main :: IO ()
main 
    = simplifyMain lookupFile
    
simplifyMain :: 
    ([String] -> IO [(FilePath, String, Problem)]) -> 
    IO ()
simplifyMain problemFactory =
    do
    reportCmdLine
    argsPre <- cmdArgs paverDefaultArgs
    let args = setDefaults argsPre
    case checkArgs args of
        [] -> 
            do
            let problemIdOpt = problemId args
            problems <- problemFactory problemIdOpt
            _results <- mapM (runProblem args) problems
            return ()
        msgs -> 
            do
            mapM_ putStrLn msgs
            error "The above errors have been identified in the command-line arguments."
    where
    runProblem args (origFilePath, name, problem)
        =
        do
        putStrLn banner
        putStrLn $ "*** applying pp_simplify on conjecture " ++ name
        _ <- mapM saveSimplification simplifications
        putStrLn $ "total simplifications count: " ++ show (length simplifications) 
        putStrLn banner
        where
        simplifications = simplifyProblem args (name, problem)
        saveSimplification (name2, form, description) =
            do
            putStrLn $ "writing file: " ++ filePath
            writeFile filePath (descriptionAsComment ++ show form)
            where
            filePath = origFilePath ++ "-" ++ name2 ++ ".form"
            descriptionAsComment =
                unlines $ map makeLineAComment $ lines description
            makeLineAComment line = "-- " ++ line
    banner = replicate 100 '*'
        
        
simplifyProblem :: Args -> (String, Problem) -> [(String, Form TermHash, String)]
simplifyProblem _args (name, problem) =
    map addName $ zip [1..] formsAndDescriptions
    where
    addName (n, (form2, description)) =
        (name ++ "-simplified" ++ show (n :: Int), form2, description)
--        where
--        reducedVarsS = 
--            concat $ map showVarName $ Set.toAscList reducedVars
--        showVarName varId =
--            map (\(_,_,_) -> )
--            $ filter (\(i,_,_) -> i == varId) box
    formsAndDescriptions = simplifyUsingSubstitutions form
    form = addHashesInForm $ problem_form problem
--    box = problem_box problem
        
    
lookupFile :: [FilePath] -> IO [(FilePath, String, Problem)]
lookupFile otherArgs@(inputPath : _) =
    do
    problems <- getProblems
    return $ map addPath problems
    where
    addPath (name, problem) = (inputPath, name, problem)
    getProblems
        | hasFormExtension inputPath = lookupForm otherArgs
        | hasPPExtension inputPath = lookupPP otherArgs
        | hasSivExtension inputPath = lookupSiv otherArgs
        | hasTptpExtension inputPath =
            error "TPTP files not supported yet"
        | otherwise =
            error $ "Unsupported file type: " ++ inputPath
              
lookupFile _ =
    do 
    reportCmdLine
    error "No problem specified."

lookupForm :: [FilePath] -> IO [(String, Problem)]
lookupForm [inputPath] =
    do
    fileContents <- readFile inputPath
    return $ form2problems $ read $ removeComments fileContents
    where
    removeComments s =
        unlines $ filter (not . isCommentLine) $ lines s
        where
        isCommentLine ('-' : '-' : _) = True
        isCommentLine _ = False
    form2problems form =
        case getBox form of
            Right box -> mkProblems (inputPath, form, box)
            Left msg -> error $ "PolyPaver: " ++ show msg
lookupForm _ = return []

lookupPP :: [FilePath] -> IO [(String, Problem)]
lookupPP [inputPath] =
    do
    fileContents <- readFile inputPath
    let pp = parseVCInFile inputPath fileContents
    return $ mkProblems pp
    
lookupPP [inputPath, conclNumberS] =
    case reads conclNumberS of
        (conclNumber, _) : _ ->
            do
            fileContents <- readFile inputPath
            let pp = parseVCInFile inputPath fileContents
            return $ [mkProblems pp !! (conclNumber - 1)]
        _ -> ppArgsError
lookupPP _ = ppArgsError 

ppArgsError :: IO a
ppArgsError =
    do
    reportCmdLine 
    error "Expecting arguments: <file.pp> [<conclusion number>]"
    
    
lookupSiv :: [FilePath] -> IO [(String, Problem)]
lookupSiv [inputPath] =
    do
    fileContents <- readFile inputPath
    let vcs = parseSivAll inputPath fileContents
    return $ concat $ map mkProblems vcs

lookupSiv [inputPath, vcName] =
    do
    fileContents <- readFile inputPath
    return $ parseTheVC fileContents
    where
    parseTheVC fileContents = 
        mkProblems $ parseSivVC inputPath fileContents vcName

lookupSiv [inputPath, vcName, conclNumberS] =
    case reads conclNumberS of
        (conclNumber, _) : _ ->
            do
            problems <- lookupSiv [inputPath, vcName]
            return $ [problems !! (conclNumber - 1)]
        _ -> sivArgsError
    
lookupSiv _ = sivArgsError 

sivArgsError :: IO b
sivArgsError =
    do
    reportCmdLine
    error "Expecting arguments: <file.siv> [<vc name> [<conclusion number>]]"

mkProblems :: (String, Form (), [(Int, (Rational, Rational), Bool)])
                -> [(String, Problem)]
mkProblems (name, vc, box) =
    map mkProb $ zip [1..] subvcs
    where
    mkProb (conclusionNumber, subvc) = 
        (name ++ "-" ++ show (conclusionNumber :: Int), Problem box subvc)
    subvcs = splitConclusion vc 
    

hasHsExtension :: FilePath -> Bool
hasHsExtension path = ".hs" `isSuffixOf` path

hasFormExtension :: FilePath -> Bool
hasFormExtension path = ".form" `isSuffixOf` path

hasPPExtension :: FilePath -> Bool
hasPPExtension path = ".pp" `isSuffixOf` path

hasSivExtension :: FilePath -> Bool
hasSivExtension path = ".siv" `isSuffixOf` path

hasTptpExtension :: FilePath -> Bool
hasTptpExtension path = ".tptp" `isSuffixOf` path
