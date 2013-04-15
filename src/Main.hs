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

import PolyPaver.Form (splitConclusion)
import PolyPaver.Invocation
import PolyPaver.Input.SPARK
import PolyPaver.DeriveBounds

import Data.List

main 
    = defaultParsingMain lookupFile
    
lookupFile args@(inputPath : _)
    | hasFormExtension inputPath = lookupForm args
    | hasSivExtension inputPath = lookupSiv args
    | hasTptpExtension inputPath =
        error "Input of TPTP files not supported yet"  
--        lookupTptp args
    
lookupForm [inputPath] =
    do
    fileContents <- readFile inputPath
    return $ form2problem $ read fileContents
    where
    form2problem form =
        case getBox form of
            Right box -> mkProblems (inputPath, form, box)
            Left msg -> error $ "PolyPaver: " ++ show msg
    
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

sivArgsError = error "polypaver: expecting arguments: <file.siv> [<vc name> [<part number>]]"

mkProblems (name, vc, box) =
    map mkProb $ zip [1..] subvcs
    where
    mkProb (conclusionNumber, subvc) = 
        (name ++ " part " ++ show conclusionNumber, Problem box subvc)
    subvcs = splitConclusion vc 
    
hasFormExtension path = ".form" `isSuffixOf` path
hasSivExtension path = ".siv" `isSuffixOf` path
hasTptpExtension path = ".tptp" `isSuffixOf` path
