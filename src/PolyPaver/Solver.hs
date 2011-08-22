{-# LANGUAGE DeriveDataTypeable #-}

{-|
    Module      :  PolyPaver.Solver
    Description :  the main counter-example search algorihtm 
    Copyright   :  (c) Jan Duracz, Michal Konecny 
    License     :  BSD3

    Maintainer  :  jan@duracz.net
    Stability   :  experimental
    Portability :  portable

    The main counter-example search algorihtm.
-}
module PolyPaver.Solver where

import PolyPaver.Form
import PolyPaver.PPBox
import PolyPaver.Eval
import qualified PolyPaver.Logic as L

import qualified Numeric.ER.Real.Approx as RA
import Numeric.ER.Real.Approx.Interval
import Numeric.ER.Real.DefaultRepr
import qualified Data.IntMap as IMap
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes.DomainBox.IntMap
import qualified Data.Sequence as Q
import Numeric.ER.Misc

import Data.List
import Data.Maybe

import System.Console.CmdArgs
--import Control.Concurrent.STM
import System.CPUTime

data Order = 
    B | D
    deriving (Show,Data,Typeable)

data Report =
    NO | VOL
    deriving (Show,Data,Typeable)

data FPType =
    B32 | B64
    deriving (Show,Data,Typeable)


--data Constants = Constants
--    {maxdegree :: Int
--    ,maxdepth :: Int
--    ,effortindex :: Int
--    ,maxseconds :: Int
--    ,formula :: Form
--    ,intvars :: [Int]
--    ,initvolume :: IRA BM}

loop 
    order report fptype 
    origstartdeg maxdeg improvementRatioThreshold 
    maxsize
    mindepth maxdepth maxDepthReached
    ix maxtime prec form intvarids 
    queue 
    qlength inittime prevtime computedboxes 
    initvol 
    truevol
    =
    loopAux form maxDepthReached queue qlength prevtime computedboxes truevol Nothing Nothing
    where
    loopAux form maxDepthReached queue qlength prevtime computedboxes truevol maybeCurrdeg maybePrevMeasure
        | Q.null queue = do
            currtime <- getCPUTime
            putStr $
              "\nSearch complete.\nTheorem proved true in " ++
              show ((fromInteger (currtime-inittime)) / 1000000000000) ++
              " seconds.\nComputed : " ++ show computedboxes ++ 
              " boxes.\nReaching max depth : " ++ show maxDepthReached ++ "\n\n"
        | depth < mindepth = do
            putStrLn $ "initial splitting at depth " ++ show depth
            currtime <- getCPUTime
--         putStr reportSplitS
            bisectAndRecur form currtime
        | prevtime-inittime > maxtime*1000000000000 = do
            putStr $
              "\nTimeout.\nSearch aborted after " ++
              show maxtime ++
              " seconds.\nComputed : " ++ show computedboxes ++ 
              " boxes.\nReaching max depth : " ++ show maxDepthReached ++ "\n\n"
        | decided && decision = do -- formula true on this box
            currtime <- getCPUTime
            putStr reportTrueS
            loopAux
                form
                maxDepthReached 
                boxes (qlength-1) currtime (computedboxes+1) newtruevol Nothing Nothing
        | decided = do -- formula false on this box
            currtime <- getCPUTime
            putStr $
              "\nCounter example found, search aborted.\nTheorem proved false for " ++
              showBox box ++
              "\nSeconds elapsed : " ++
              show ((fromInteger (currtime-inittime)) / 1000000000000) ++
              "\nComputed  boxes : " ++ show computedboxes ++ 
              "\nMax depth : " ++ show maxDepthReached ++  
              "\nDepth : " ++ show depth ++ "\n\n"
        | currdeg < maxdeg && undecidedMeasureImproved = do -- try raising the degree before splitting
            putStrLn $ "raising degree to " ++ show (currdeg + 1)
            currtime <- getCPUTime
            loopAux
                form
                maxDepthReached 
                queue qlength currtime computedboxes truevol
                (Just $ currdeg + 1)
                (Just undecidedMeasure)
        | depth >= maxdepth ||
          splitdomL `ppEqual` splitdom || 
          splitdomR `ppEqual` splitdom   ||
          length thinvarids == dim = do -- formula undecided and cannot split any further
            currtime <- getCPUTime
            putStr $ 
              "\nCannot split box, search aborted.\nUndecided for : " ++
              showBox cbox ++
              "\nSeconds elapsed : " ++
              show ((fromInteger (currtime-inittime)) / 1000000000000) ++
              "\nComputed boxes : " ++ show computedboxes ++ 
--           "\nQueue length : " ++ show qlength ++
              "\nMaxdepth : " ++ show maxDepthReached ++  
              "\nDepth : " ++ show depth ++ "\n\n"
        | otherwise = do -- formula undecided on this box, will split it
            putStrLn $ "splitting at depth " ++ show depth ++ ", new queue size is " ++ show (qlength + 1)
            currtime <- getCPUTime
--         putStr reportSplitS
            bisectAndRecur undecidedSimplerForm currtime
            {-
            loop 
                order report fptype maxdeg maxdepth (max (depth+1) maxDepthReached) ix maxtime prec form intvarids  
                (boxes Q.|> (depth+1,boxL) Q.|> (depth+1,boxR)) 
                (qlength+1) inittime currtime (computedboxes+1) 
                initvol 
                truevol 
                breadth-first above depth-first below
            loop 
                order report fptype maxdeg maxdepth (max (depth+1) maxDepthReached) ix maxtime prec form intvarids 
                ((depth+1,boxL) Q.<| (depth+1,boxR) Q.<| boxes) 
                (qlength+1) inittime currtime (computedboxes+1) 
                initvol 
                truevol 
            -}
        where
        bisectAndRecur form currtime =
            case order of 
                B -> 
                    loopAux
                        form
                        (max (depth+1) maxDepthReached) 
                        (boxes Q.|> (depth+1,newstartdeg,boxL) Q.|> (depth+1,newstartdeg,boxR)) 
                        (qlength+1) currtime (computedboxes+1) truevol Nothing Nothing
                D ->
                    loopAux 
                        form
                        (max (depth+1) maxDepthReached) 
                        ((depth+1,newstartdeg,boxL) Q.<| (depth+1,newstartdeg,boxR) Q.<| boxes) 
                        (qlength+1) currtime (computedboxes+1) truevol Nothing Nothing
        newstartdeg =
            (origstartdeg + currdeg) `div` 2
        currdeg =  
            case maybeCurrdeg of Just currdeg -> currdeg; _ -> startdeg
        reportTrueS =
            case report of
                 VOL -> 
                     "Proved fraction : " ++ show newtruevol ++ "\n"
                 NO -> 
                     ""
{-      reportSplitS =
            case report of
                 VOL -> 
                     "Proved fraction : " ++ show truevol ++ "\n"
                 NO -> 
                     ""-}
        (depth,startdeg,box) = Q.index queue 0
        boxes = Q.drop 1 queue
        decided = isJust maybeValue
        decision = fromJust maybeValue
        dim = DBox.size box
        splitdom = DBox.lookup "looking up splitdom in solver" splitvar box
        splitdomL = DBox.lookup "looking up splitdom in solver" splitvar boxL
        splitdomR = DBox.lookup "looking up splitdom in solver" splitvar boxR
         
        (splitvar,(boxL,boxR)) = L.split thinvarids box value
        maybeValue = L.decide dim value
        value = 
            case fptype of
                 B32 -> evalForm currdeg maxsize ix cbox (23,-126) form :: L.TVM -- Maybe Bool
                 B64 -> evalForm currdeg maxsize ix cbox (52,-1022) form :: L.TVM -- Maybe Bool
        (L.TVMUndecided undecidedMeasure undecidedSimplerForm) = value
        undecidedMeasureImproved = 
            case maybePrevMeasure of
                Nothing -> True
                Just prevUndecidedMeasure ->
                    prevUndecidedMeasure / undecidedMeasure > improvementRatioThreshold
        thinvarids = DBox.keys thincbox
        thincbox = DBox.filter (ppCoeffsZero . snd) cbox -- thin subbox of contracted box
        cbox = box -- contractIntVarDoms box intvarids -- box with contracted integer doms
        newtruevol = truevol+(ppVolume box)/initvol

volume box =
  DBox.fold (\dom vol -> vol * width dom) 1 box

width dom =
  domR-domL
  where
  (domL,domR) = RA.bounds dom

--enqueueBox stateTV rgba box =
--    atomically $ do
--        state <- readTVar stateTV
--        writeTVar stateTV ((box,rgba) : state)

showBox :: PPBox BM -> String
showBox =
  DBox.foldWithKey 
    (\varid vardom boxStr -> "\nx" ++ 
     show varid ++ " in " ++ 
     show vardom ++ " " ++ boxStr) 
    ""

--contractIntVarDoms :: PPBox BM -> [Int] -> PPBox BM
--contractIntVarDoms initbox intvarids =
--    foldl'
--        (
--            \box intvarid 
--            -> 
--            IMap.adjust contractIntVarDom intvarid box
--        )
--        initbox
--        intvarids
--
--{-
--    WARNING: uses floor and therefore
--    RA.doubleBounds
---}
--contractIntVarDom :: (IRA BM, Map.Map Int (IRA BM)) -> (IRA BM, Map.Map Int (IRA BM))
--contractIntVarDom dom
--    | domLclg <= domRflr =
--        ERInterval (fromInteger domLclg) (fromInteger domRflr)
--    | otherwise =
--        RA.bottomApprox
--    where
----    (domLflr,domRclg) = RA.integerBounds dom
--    domLclg = ceiling domL  
--    domRflr = floor domR
--    domL = erintv_left dom
--    domR = erintv_right dom

{-|  conclusion of their_sqrt_25: degree 40

*(x, *** 40
    *( *** 39
        *(1 / 2,  *** 13
            *(
                *(1 / 2, 
                    *(
                        *(1 / 2, t__1), 
                        -(3, 
                            *(*(t__1, t__1), x)
                         )
                     )
                 ), 
                 -(3, 
                    *(
                        *(
                            *(
                                *(1 / 2, t__1), 
                                -(3, 
                                    *(
                                        *(t__1, t__1), 
                                        x
                                    )
                                )
                            ), 
                            *(
                                *(1 / 2, t__1), 
                                -(3, 
                                    *(
                                        *(t__1, t__1), 
                                        x
                                    )
                                )
                            )
                        ), 
                        x
                    )
                )
            )
        ), 
        -(3, 
            *(
                *( *** 26
                    *( *** 13
                        *( *** 4
                            1 / 2, 
                            *(
                                *(1 / 2, t__1), 
                                -(3, 
                                    *(
                                        *(t__1, t__1), 
                                        x
                                    )
                                )
                            )
                        ), 
                        -(3, 
                            *( *** 9
                                *( *** 8
                                    *( *** 4
                                        *(1 / 2, t__1), 
                                        -(3, 
                                            *(
                                                *(t__1, t__1), 
                                                x
                                            )
                                        )
                                    ), 
                                    *( *** 4
                                        *(1 / 2, t__1), 
                                        -(3, 
                                            *(
                                                *(t__1, t__1), 
                                                x
                                            )
                                        )
                                    )
                                ), 
                                x
                            )
                        )
                    ), 
                    *( *** 13
                        *(1 / 2, 
                            *( *** 4
                                *(1 / 2, t__1), 
                                -(3, 
                                    *(
                                        *(t__1, t__1), 
                                        x
                                    )
                                )
                            )
                        ), 
                        -(3, 
                            *( *** 9
                                *( *** 8
                                    *( *** 4
                                        *(1 / 2, t__1), 
                                        -(3, 
                                            *(
                                                *(t__1, t__1), 
                                                x
                                            )
                                        )
                                    ), 
                                    *( ***4
                                        *(1 / 2, t__1), 
                                        -(3, 
                                            *( ***3
                                                *(t__1, t__1)
                                                ,x
                                            )
                                        )
                                    )
                                ), 
                                x
                            )
                        )
                    )
                ), 
                x
            )
        )
    )
)
-}