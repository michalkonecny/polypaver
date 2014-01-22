{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

module PolyPaver.Simplify.Args 
(
    Args,
    PP_Simplify(..),    
    Order(..),
    paverDefaultArgs,
    setDefaults,
    checkArgs   
)
where

import System.Console.CmdArgs hiding (args)
import qualified System.Console.CmdArgs as Args

--import Data.Typeable
--import Data.Data
--import Data.Maybe (catMaybes)

type Args = PP_Simplify

data PP_Simplify = PP_Simplify 
    {problemId :: [String]
    ,quiet :: Bool
    ,verbose :: Bool
    }
    deriving (Show,Data,Typeable)

data Order = 
    BFS | DFS 
--    | DFSthenBFS | BFSFalsifyOnly
    deriving (Show,Data,Typeable)

paverDefaultArgs :: Args
paverDefaultArgs =
    PP_Simplify
    {problemId = [] &= Args.args &= typ "PROBLEM_ID" 
    ,quiet = False &= help "suppress all output except the final result (default off)" 
        &= groupname "Verbosity"
    ,verbose = False &= help "output extra details while paving (default off)"
    } 
    &= help (unlines 
                ["Tries to simplify the given conjecture(s), while making them stronger.",
                 "Writes each discovered simplification in a .form file.",
                 "Proving any of the simplifications proves the original.",
                 "DISPROVING A SIMPLIFICATION DOES NOT DISPROVE THE ORIGINAL.",
                 "",
                 "[PROBLEM_ID] specifies one or more conjectures as follows:                   ",
                 "  <name>.vc [<conclusion number>]: like a VC in SPARK .siv                   ",
                 "  <name>.siv [<vc name> [<conclusion number>]]: SPARK-generated VCs          ",
                 "  <name>.form: using internal syntax (machine generated)                     "
                 ])
    &= summary "pp_simplify 0.3 (c) 2014 Michal Konecny (Aston University)"
    &= name "pp_simplify"

setDefaults :: PP_Simplify -> PP_Simplify
setDefaults = id 
--    setMaxQLength . setStartDegree
--    where
--    setMaxQLength args =
--        if maxQueueLength args /= -1
--            then args -- maxQueueLength is explicitly set, do no change
--            else 
--                case order args of
--                    DFS -> args { maxQueueLength = maxQueueLengthDefaultDFS }
--                    BFS -> args { maxQueueLength = maxQueueLengthDefaultBFS }
----                    DFSthenBFS -> args { maxQueueLength = maxQueueLengthDefaultDFS }
----                    BFSFalsifyOnly -> args { maxQueueLength = maxQueueLengthDefaultBFS }
--    setStartDegree args 
--        | startDegree args == -1 = args { startDegree = degree args }
--        | otherwise = args 

checkArgs :: Args -> [String]
checkArgs _args =
    [] 
--    catMaybes [checkSplitGuessing, checkSkewing]
--    where
--    checkSkewing
--        | boxSkewing args && startDegree args == 0 =
--            Just $
--                 "Box skewing is not compatible with polynomial degree 0."
--                 ++ "\n  Raise starting polynomial degree to a positive value."
--        | otherwise = Nothing
--    checkSplitGuessing 
--        | splitGuessing args /= -1 && startDegree args == 0 =
--            Just $
--                 "Guessing split direction is not compatible with polynomial degree 0."
--                 ++ "\n  Raise starting polynomial degree to a positive value."
--        | otherwise = Nothing
        

