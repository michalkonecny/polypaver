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

import PolyPaver.Paver
import PolyPaver.Input.SPARK

import Data.List

main 
    = defaultParsingMain lookupSiv
    
lookupSiv [inputPath]
    | hasSivExtension inputPath =
    do
    fileContents <- readFile inputPath
    let vcs = parseSivAll inputPath fileContents
    return $ map mkProb vcs
    where
    mkProb (name, vc, box) = (name, Problem box vc)
    
lookupSiv [inputPath, vcName]
    | hasSivExtension inputPath =
    do
    fileContents <- readFile inputPath
    let (_, vc, box) = parseSivVC inputPath fileContents vcName
    return $ [(vcName, Problem box vc)]
    
    
lookupSiv _ = argsError 

argsError = error "polypaver: expecting arguments: <file.siv> <vc name>"

hasSivExtension path = ".siv" `isSuffixOf` path
hasTptpExtension path = ".tptp" `isSuffixOf` path
