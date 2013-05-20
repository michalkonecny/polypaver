{-# LANGUAGE CPP #-}
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
import PolyPaver.Input.TPTP

#ifdef DynamicLoading 
import PolyPaver.Input.Haskell
#endif
import PolyPaver.DeriveBounds (getBox)
import PolyPaver.Vars (substituteVarsForm,getFormVarNames)

import System.Console.CmdArgs

import qualified Data.IntMap as IMap
import Data.List

main 
    = batchMain lookupFile
    
lookupFile args@(inputPath : _)
#ifdef DynamicLoading 
    | hasHsExtension inputPath = lookupHaskell args
#endif
    | hasFormExtension inputPath = lookupForm args
    | hasPPExtension inputPath = lookupPP args
    | hasSivExtension inputPath = lookupSiv args
    | hasTptpExtension inputPath =
        error "Input of TPTP files not supported yet"  
--        lookupTptp args
    
#ifdef DynamicLoading     
lookupHaskell [inputPath] = lookupHaskell [inputPath, "problem"]
lookupHaskell (inputPath : problemNames) =
    do
    problems <- loadHaskellProblems inputPath problemNames
    return $ zip problemNames problems
#endif
    
lookupForm [inputPath] =
    do
    tightnessValues <- getTightnessValues
    fileContents <- readFile inputPath
    return $ form2problems tightnessValues $ read fileContents
    where
    form2problems tightnessValues form =
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
                    Just $ concat $ map mkProblems $ map substT tightnessValues
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

ppArgsError = error "PolyPaver: expecting arguments: <file.pp> [<conclusion number>]"
    
    
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

sivArgsError = error "PolyPaver: expecting arguments: <file.siv> [<vc name> [<conclusion number>]]"

mkProblems (name, vc, box) =
    map mkProb $ zip [1..] subvcs
    where
    mkProb (conclusionNumber, subvc) = 
        (name ++ " conclusion " ++ show conclusionNumber, Problem box subvc)
    subvcs = splitConclusion vc 
    
hasHsExtension path = ".hs" `isSuffixOf` path
hasFormExtension path = ".form" `isSuffixOf` path
hasPPExtension path = ".pp" `isSuffixOf` path
hasSivExtension path = ".siv" `isSuffixOf` path
hasTptpExtension path = ".tptp" `isSuffixOf` path
