{-# LANGUAGE DeriveDataTypeable #-}

{-|
    Module      :  PolyPaver.GenerateMain
    Description :  tools to generate PolyPaver Main modules 
    Copyright   :  (c) Jan Duracz, Michal Konecny 
    License     :  BSD3

    Maintainer  :  jan@duracz.net
    Stability   :  experimental
    Portability :  portable

    Tools to generate PolyPaver Main modules.
-}
module PolyPaver.GenerateMain
(
    writePolyPaverMain
)
where

import PolyPaver.PPBox
import PolyPaver.Form

import qualified System.FilePath as FP

writePolyPaverMain :: 
    FP.FilePath ->
    (String, Form, [(Int, (Rational, Rational))]) ->
    IO ()
writePolyPaverMain outputFolder (name, form, box) =
    do
    writeFile outputFile mainS 
    where
    outputFile = outputFolder `FP.combine` (name ++ ".hs")
    mainS =
        unlines $
            [
             "module Main(main) where"
            ,""
            ,"import PolyPaver"
            ,"import Data.Ratio ((%))"
            ,""
            ,"main ="
            ,"    defaultMain Problem"
            ,"        {"
            ,"          box = " ++ show box
--            ,"          ,ivars = []"
            ,"          ,theorem = thm"
            ,"        }"
            ,"thm ="
            ,"    " ++ show form
            ,""
            ]       

