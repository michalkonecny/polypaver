{-# LANGUAGE DeriveDataTypeable #-}

{-|
    Module      :  Main
    Description :  the polypaver executable  
    Copyright   :  (c) Jan Duracz, Michal Konecny 
    License     :  BSD3

    Maintainer  :  jan@duracz.net
    Stability   :  experimental
    Portability :  portable

    The polypaver executable.
-}
module Main where

import PolyPaver.Args
import PolyPaver.Invocation
import PolyPaver.Form
import PolyPaver.Input.SPARK
import PolyPaver.Input.TPTP (parseTPTP)

--import Numeric.ER.Real.DefaultRepr

import PolyPaver.DeriveBounds (getBox)
import PolyPaver.Vars (substituteVarsForm,getFormVarNames)

--import System.Console.CmdArgs

import qualified Data.IntMap as IMap
import Data.List

main :: IO ()
main 
    = batchMain lookupFile
    
lookupFile :: Args -> [FilePath] -> IO [(String, Problem)]
lookupFile args otherArgs@(inputPath : _)
    | hasFormExtension inputPath = lookupForm args otherArgs
    | hasPPExtension inputPath = lookupPP otherArgs
    | hasSivExtension inputPath = lookupSiv otherArgs
    | hasTptpExtension inputPath = lookupTptp otherArgs
lookupFile _ _ =
    do 
    reportCmdLine
    error "No problem specified."


lookupForm :: Args -> [FilePath] -> IO [(String, Problem)]
lookupForm args [inputPath] =
    do
    fileContents <- readFile inputPath
    return $ form2problems $ read $ removeComments fileContents
    where
    removeComments s =
        unlines $ filter (not . isCommentLine) $ lines s
        where
        isCommentLine ('-' : '-' : _) = True
        isCommentLine _ = False
    tightnessVals = getTightnessValues args
    form2problems form =
        case getBox form of
            Right box -> mkProblems (inputPath, form, box)
            Left msg ->
                case tryWithT of
                    Just problems -> problems
                    Nothing -> error $ "PolyPaver: " ++ show msg
        where
        tryWithT = 
            case IMap.lookup 0 varNames of
                Just name | name == "T" ->
                    Just $ concat $ map mkProblems $ map substT tightnessVals
                _ -> Nothing
        varNames = getFormVarNames form
        substT t =
            case getBox formT of
                Right box -> (inputPath ++ "-T=" ++ show t, formT, box)
                Left msg -> error $ "PolyPaver: " ++ show msg
            where
            formT = substituteVarsForm s form
            s varId | varId == 0 = Just $ Lit $ fromInteger t
            s _ = Nothing
--        ts = reverse $ take 11 $ iterate (*2) 1
lookupForm _ _ = return []

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

lookupTptp :: [FilePath] -> IO [(String, Problem)]
lookupTptp [inputPath] =
    do
    fileContents <- readFile inputPath
    let tptp = parseTPTP inputPath fileContents
    return $ concat $ map mkProblems tptp
lookupTptp _ = ppArgsError 

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
