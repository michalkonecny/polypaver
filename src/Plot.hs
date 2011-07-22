
module Plot where

import Control.Concurrent
import Control.Concurrent.STM

----canvas-----

import Graphics.UI.Gtk hiding (drawPolygon)
import Graphics.Rendering.Cairo
import Data.Time.Clock.POSIX
import Time

----prover-----

import Data.Maybe
import qualified Numeric.ER.Real.Approx as RA
import Numeric.ER.Real.DefaultRepr
import qualified Numeric.ER.BasicTypes.DomainBox.IntMap as DBox
import qualified Numeric.ER.BasicTypes.DomainBox.IntMap as IMap
import qualified Data.Sequence as Q
--import System.Environment
import Prover

--main = do
--    stateTV <- atomically $ newTVar []
--    forkIO $ prover maxdeg form (Q.singleton initbox) stateTV        
--    myCanvas stateTV draw 200 200

{-
    This bisection search loop enqueues decided true boxes 
    until the stack is empty or a false box is found.
-}
plotter maxdeg form queue stateTV 
    | Q.null queue =
        putStrLn "Search complete.\nVC proved true."
    | decided && decision = do -- draw green box
        enqueueBox stateTV (0,1,0,0.4) box
--        putStrLn "VC true over"
--        print box
        plotter maxdeg form boxes stateTV
    | decided = do -- draw red box
        enqueueBox stateTV (1,0,0,0.9) box
        putStrLn "Counter example found, search aborted.\nVC false over"
        print box
--        loop maxdeg form boxes true (box:false)
--         prover maxdeg form boxes stateTV
    | otherwise = do -- draw transparent sub boxes
        mapM_ (enqueueBox stateTV (0,0,0,0)) [boxL,boxR]
        plotter maxdeg form (boxes Q.|> boxL Q.|> boxR) stateTV
    where
    box = Q.index queue 0
    boxes = Q.drop 1 queue
    decided = isJust value
    decision = fromJust $ value
    value = evalForm maxdeg box form
    (boxL,boxR) = DBox.split box var pt
    (var,pt) = DBox.bestSplit box

{-
    This bisection search loop enqueues decided true boxes 
    until the stack is empty or a false box is found.
-}
prover maxdeg form queue 
    | Q.null queue =
        putStrLn "Search complete.\nVC proved true."
    | decided && decision = do -- draw green box
--        enqueueBox stateTV (0,1,0,0.4) box
--        putStrLn "VC true over"
--        print box
        prover maxdeg form boxes
    | decided = do -- draw red box
--        enqueueBox stateTV (1,0,0,0.9) box
        putStrLn "Counter example found.\nVC false over"
        print box
--        loop maxdeg form boxes true (box:false)
        prover maxdeg form boxes
    | otherwise = do -- draw transparent sub boxes
--        mapM_ (enqueueBox stateTV (0,0,0,0)) [boxL,boxR]
        prover maxdeg form (boxes Q.|> boxL Q.|> boxR)
    where
    box = Q.index queue 0
    boxes = Q.drop 1 queue
    decided = isJust value
    decision = fromJust $ value
    value = evalForm maxdeg box form
    (boxL,boxR) = DBox.split box var pt
    (var,pt) = DBox.bestSplit box

enqueueBox stateTV rgba box  =
    atomically $ do
        state <- readTVar stateTV
        writeTVar stateTV ((box,rgba) : state)

draw initbox w h state  = do
    setSourceRGB 1 1 1
    rectangle 0 0 w h
    fill
--    drawSubBox initbox w h subbox
    mapM_ (drawSubBox initbox w h) state
--     mapM_ (drawSubBoxMidpoint initbox w h) state

-- drawSubBoxMidpoint box w h subbox = do
--     setSourceRGB 0 0 0
--     rectangle midx (h-midy) 1 1
--     fill
--     where
--     midx = 0.5*(cbxL+cbxR)
--     midy = 0.5*(cbyL+cbyR)
--     [(cbxL,cbxR),(cbyL,cbyR)] = subBoxToCanvasBox box w h subbox

drawSubBox box w h (subbox,(r,g,b,a)) = do
    setSourceRGBA r g b a -- 0 1 0 0.4 light green
    rect 
    fill
    setSourceRGBA 0 0 0 1 -- black border
    setLineWidth 0.5
    rect
    stroke
    where
    rect = rectangle cbxL (h-cbyR) (cbxR-cbxL) (cbyR-cbyL)
    [(cbxL,cbxR),(cbyL,cbyR)] = subBoxToCanvasBox box w h subbox

{-
    takes the sub box subbox [(sbxL,sbxR),(sbyL,sbyR)] of 
    box and maps it to a sub box [(cbxL,cbxR),(cbyL,cbyR)] 
    of a canvas of width w and height h
-}
subBoxToCanvasBox box w h subbox =
    [(cbxL,cbxR),(cbyL,cbyR)]
    where
    [(cbxL,cbyL),(cbxR,cbyR)] = 
        map (boxCoordsToCanvasCoords box w h) [(sbxL,sbyL),(sbxR,sbyR)]
    ((sbxL,sbxR):(sbyL,sbyR):_) = 
        map RA.doubleBounds $ DBox.elems subbox

{-
    takes a point (bx,by) in box and maps it to a point
    (cx,cy) in a canvas of width w and height h
-}
boxCoordsToCanvasCoords box w h (bx,by) =
    (cx,cy)
    where
    cx = (bx-bxL)*w/(bxR-bxL)
    cy = (by-byL)*h/(byR-byL)
    ((bxL,bxR):(byL,byR):_) = map RA.doubleBounds $ DBox.elems box

myCanvas stateTV draw w h = do
    unsafeInitGUIForThreadedRTS
    window <- windowNew
    da <- drawingAreaNew
    set window [ containerChild := da ]
    windowSetDefaultSize window w h
    onExpose da (myExposeHandler stateTV da draw)
    timeoutAdd (widgetQueueDraw da >> return True) 1000 >> return ()
    idleAdd (yield >> return True) priorityDefaultIdle >> return ()
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI

myExposeHandler stateTV widget draw e = do
    drawWin <- widgetGetDrawWindow widget
    (wi,hi) <- widgetGetSize widget
    let (w,h) = (realToFrac wi, realToFrac hi)
    state <- atomically $ readTVar stateTV
    renderWithDrawable drawWin $ do
        draw w h state -- (map realToFrac state)
    return True

