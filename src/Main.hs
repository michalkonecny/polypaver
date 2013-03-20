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

import Data.List

main 
    = defaultParsingMain lookupSiv
    
lookupSiv [inputPath]
    | hasSivExtension inputPath =
    do
    fileContents <- readFile inputPath
    let vcs = parseSivAll inputPath fileContents
    return $ concat $ map mkProblems vcs

lookupSiv [inputPath, vcName]
    | hasSivExtension inputPath =
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
        _ -> argsError
    
lookupSiv _ = argsError 

argsError = error "polypaver: expecting arguments: <file.siv> [<vc name> [<part number>]]"

mkProblems (name, vc, box) =
    map mkProb $ zip [1..] subvcs
    where
    mkProb (conclusionNumber, subvc) = 
        (name ++ " part " ++ show conclusionNumber, Problem box subvc)
    subvcs = splitConclusion vc 
    
hasSivExtension path = ".siv" `isSuffixOf` path
hasTptpExtension path = ".tptp" `isSuffixOf` path
