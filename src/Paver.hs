{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import System.Console.CmdArgs

data Paver = Paver 
    {degree :: Int
    ,depth :: Int
    ,time :: Int}
    deriving (Show,Data,Typeable)

paver = Paver 
    {degree = 0 &= help "Maximum polynomial bound degree"
    ,depth = 10 &= help "Maximum subdivision depth"
    ,time = 3600 &= help "Maximum solving time in seconds"}

main = print =<< cmdArgs paver
