{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

module PolyPaver.Args 
(
    Args,
    PolyPaver(..),    
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

type Args = PolyPaver

data PolyPaver = PolyPaver 
    {problemId :: [String]
    ,tightnessValues :: String
    ,startDegree :: Int -- maximum polynomial degree for the first attempt, -1 means startDegree = degree
    ,degree :: Int -- maximum polynomial degree to try
    ,maxSize :: Int  -- maximum number of terms in a polynomial
    ,effort :: Int  -- effort index for AERN
    ,minIntegrExp :: Int
    ,order :: Order
    ,splitIntFirst :: Bool
    ,minDepth :: Int -- minimum bisection depth
    ,maxDepth :: Int -- maximum bisection depth
    ,maxQueueLength :: Int -- maximum queue length
    ,time :: Int -- timeout in seconds
--    ,boxSkewing :: Bool
--    ,splitGuessing :: Int
    ,quiet :: Bool
    ,verbose :: Bool
    ,plotWidth :: Int
    ,plotHeight :: Int
    }
    deriving (Show,Data,Typeable)

data Order = 
    BFS | DFS 
--    | DFSthenBFS | BFSFalsifyOnly
    deriving (Show,Data,Typeable)

paverDefaultArgs :: Args
paverDefaultArgs =
    PolyPaver 
    {problemId = [] &= Args.args &= typ "PROBLEM_ID" 
    ,tightnessValues = "1" &= name "i"
        &= groupname "Problem parameters"
        &= help "value(s) of T to try (if the formula has an unbound variable T) (eg \"2^0..10\" or \"1..10\" or \"1,10,100\") (default = 1)" 
    ,startDegree = -1 &= name "s" &= help "first polynomial degree to try on each box (default = degree)"
        &= groupname "Box solving effort"
    ,degree = 0 &= name "d" &= help "maximum polynomial degree (default = 0)" 
    ,maxSize = 100 &= name "z" &= help "maximum polynomial term size (default = 100)"
    ,effort = 10 &= help "for approximating point-wise sqrt and exp (default = 10)" 
    ,minIntegrExp = 0 &= name "I" &= help "n to compute approximate integration step using 2^(-n)" 
    ,order = DFS 
        &= groupname "Box subdivision strategy"
        &= help "sub-problem processing order, bfs for breadth-first or dfs for depth-first, (default = dfs)"
    ,splitIntFirst = False &= name "f" 
        &= help "split integer valued domains until they are exact before splitting the continuous domains"
    ,minDepth = 0 &= help "minimum bisection depth (default = 0)"
    ,maxDepth = 1000 &= name "b" &= help "maximum bisection depth (default = 1000)"
    ,maxQueueLength = -1 &= name "u" 
        &= help ("maximum queue size (default = " 
                    ++ show maxQueueLengthDefaultDFS ++ " for depth-first and "
                    ++ show maxQueueLengthDefaultBFS ++ " for breadth-first order)")
    ,time = 7*24*3600 &= help "timeout in seconds (default = 7*24*3600 ie 1 week)"    
--    ,boxSkewing = False &= name "k" &= help "allow parallelepiped boxes, by default only coaxial rectangles" 
--        &= groupname "Experimental"
--    ,splitGuessing = -1 &= name "g" &= opt (20 :: Int) &= help "try guessing the best box splitting direction but do not allow a box in which a pair of box edge lengths exceeds a given ratio (default 20)"
--    ,epsrelbits = 23 &= name "r" &= help "n to compute machine epsilon using 2^-n (default = 24)" &= groupname "Floating point rounding interpretation in conjectures"
--    ,epsabsbits = 126 &= name "a" &= help "n to compute denormalised epsilon using 2^-n (default = 126)"
    ,quiet = False &= help "no reporting of progress on the console (default off)" 
        &= groupname "Verbosity"
    ,verbose = False &= help "report extra progress details on the console (default off)"
    ,plotWidth = 0 &= name "w" &= help "plot width for 2D problems, 0 mean no plotting (default)" 
        &= groupname "Plotting"
    ,plotHeight = 0 &= name "h" &= help "plot height for 2D problems, 0 mean no plotting (default)"
    } 
    &= help (unlines 
                ["Tries to decide numerical conjectures (problems) using polynomial enclosures.",
                 "[PROBLEM_ID] specifies one or more conjectures as follows:                   ",
                 "  <name>.pp [<conclusion number>]: like a single VC in SPARK .siv            ",
                 "  <name>.siv [<vc name> [<conclusion number>]]: SPARK-generated VCs          ",
                 "  <name>.form: using internal syntax (machine generated)                     "
#ifdef DynamicLoading 
                 , 
                 "  <name>.hs [<identifier name>]: Haskell constant of type Problem            "
#endif
                 ])
    &= summary "PolyPaver 0.3 (c) 2015 Jan Duracz and Michal Konecny (Aston University)"
    &= name "polypaver"

setDefaults :: PolyPaver -> PolyPaver
setDefaults = setMaxQLength . setStartDegree
    where
    setMaxQLength args =
        if maxQueueLength args /= -1
            then args -- maxQueueLength is explicitly set, do no change
            else 
                case order args of
                    DFS -> args { maxQueueLength = maxQueueLengthDefaultDFS }
                    BFS -> args { maxQueueLength = maxQueueLengthDefaultBFS }
--                    DFSthenBFS -> args { maxQueueLength = maxQueueLengthDefaultDFS }
--                    BFSFalsifyOnly -> args { maxQueueLength = maxQueueLengthDefaultBFS }
    setStartDegree args 
        | startDegree args == -1 = args { startDegree = degree args }
        | otherwise = args 

maxQueueLengthDefaultDFS :: Int
maxQueueLengthDefaultDFS = 50
maxQueueLengthDefaultBFS :: Int
maxQueueLengthDefaultBFS = 5000

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
        

