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
import PolyPaver.Invocation (Problem(..), reportCmdLine)
import PolyPaver.Form (Form, splitConclusion)
import PolyPaver.Input.SPARK
import PolyPaver.DeriveBounds (getBox)

--import Numeric.ER.Real.DefaultRepr

--import qualified Data.IntMap as IMap
import Data.List

main :: IO ()
main 
    = undefined
--    = simplifyMain lookupFile
    
lookupFile :: [FilePath] -> IO [(String, Problem)]
lookupFile otherArgs@(inputPath : _)
    | hasFormExtension inputPath = lookupForm otherArgs
    | hasPPExtension inputPath = lookupPP otherArgs
    | hasSivExtension inputPath = lookupSiv otherArgs
    | hasTptpExtension inputPath =
        error "Input of TPTP files not supported yet"  
--        lookupTptp args
lookupFile _ =
    do 
    reportCmdLine
    error "No problem specified."

lookupForm :: [FilePath] -> IO [(String, Problem)]
lookupForm [inputPath] =
    do
    fileContents <- readFile inputPath
    return $ form2problems $ read fileContents
    where
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
        (name ++ " conclusion " ++ show (conclusionNumber :: Int), Problem box subvc)
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
