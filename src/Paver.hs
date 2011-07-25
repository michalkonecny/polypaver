{-# LANGUAGE DeriveDataTypeable #-}
module Paver where
import System.Console.CmdArgs

data Paver = Paver 
    {bits :: Int
    ,degree :: Int
    ,depth :: Int
    ,timeout :: Int
    }
    deriving (Data,Typeable,Show,Eq)

paver = Paver
    {bits = 32 &= help ""

