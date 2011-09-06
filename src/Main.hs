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

main 
    = defaultParsingMain lookupSiv
    
lookupSiv [inputPath, vcName]
    =
    do
    fileContents <- readFile inputPath
    let (vc, box) = parseSiv inputPath fileContents vcName
    return $ Problem box vc
    
    
lookupSiv _ = argsError 

argsError = error "polypaver: expecting arguments: <file.siv> <vc name>"