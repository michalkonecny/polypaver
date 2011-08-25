
module PolyPaver.Plot 
(
    initPlot,
    addBox,
    waitForClose
)
where

import PolyPaver.PPBox

import qualified Numeric.ER.Real.Approx as RA
import Numeric.ER.Misc

import Control.Concurrent
import Control.Concurrent.STM

----canvas-----
import Graphics.UI.Gtk hiding (drawPolygon)
import Graphics.Rendering.Cairo
import Data.Time.Clock.POSIX
import Time

{-|
  sample usage: @addBox stateTV (1,0,0,0.9) box@
-}
addBox stateTV rgba box  
    =
    do
--    putStrLn $ "addBox called with " ++ ppShow box
    atomically $ do
        state <- readTVar stateTV
        writeTVar stateTV ((box,rgba) : state)

waitForClose stateTV =
    atomically $
        do
        state <- readTVar stateTV
        case state of
            [] -> return ()
            _ -> retry

initPlot initbox w h =
    do
    stateTV <- atomically $ newTVar []
    forkIO $ myCanvas stateTV draw initboxInfo w h
    return stateTV
    where
    -- 4---3
    -- |   |
    -- 1---2
    [p1@[x1,y1],p4@[_,y4],p2@[x2,_],p3] = ppCorners initbox
    initboxInfo = (ppCentre initbox, x2 - x1, y4 - y1)

myCanvas stateTV draw initboxInfo w h = 
    do
    unsafeInitGUIForThreadedRTS
    window <- windowNew
    da <- drawingAreaNew
    set window [ containerChild := da ]
    windowSetDefaultSize window w h
    onExpose da (myExposeHandler stateTV da draw initboxInfo)
    timeoutAdd (widgetQueueDraw da >> return True) 500 >> return ()
    idleAdd (yield >> return True) priorityDefaultIdle >> return ()
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
    atomically $ writeTVar stateTV []

myExposeHandler stateTV widget draw initboxInfo _event = 
    do
--    putStrLn $ "myExposeHandler called"
    drawWin <- widgetGetDrawWindow widget
    (wi,hi) <- widgetGetSize widget
    let (w,h) = (realToFrac wi, realToFrac hi)
    state <- atomically $ readTVar stateTV
    renderWithDrawable drawWin $ 
        do
        draw initboxInfo w h state
    return True

draw initboxInfo w h state  = 
    do
    mapM_ (drawSubBox initboxInfo w h) $ reverse state

drawSubBox initboxInfo w h (subbox,(r,g,b,a)) = 
    do
    setSourceRGBA r g b a -- 0 1 0 0.4 light green
    parallelepiped
    fill
    setSourceRGBA 0 0 0 1 -- black border
    setLineWidth 0.5
    parallelepiped
    stroke
    where
    parallelepiped =
        do
        (uncurry moveTo) p1
        mapM_ (uncurry lineTo) [p2,p3,p4]
        closePath
    [p1,p2,p3,p4] = subBoxToCanvasBox initboxInfo w h subbox

{-
    Takes a box in the problem format and maps it to 
    a box in the coordinates of a canvas of the given size.
-}
subBoxToCanvasBox initboxInfo w h subbox =
    map converPt [p1,p2,p3,p4]
    where
    converPt [x,y] =
        boxCoordsToCanvasCoords initboxInfo w h (x,y)
    [p1,p4,p2,p3] = ppCorners subbox

{-
    Takes a point in problem coordinates and maps it to a point
    in the coordinates of a canvas of width w and height h.
-}
boxCoordsToCanvasCoords initboxInfo w h (xB,yB) =
--    unsafePrint
--    (
--        "boxCoordsToCanvasCoords: "
--        ++ "\n w = " ++ show w
--        ++ "\n h = " ++ show h
--        ++ "\n wI = " ++ show wI
--        ++ "\n hI = " ++ show hI
--        ++ "\n xIO,yIO = " ++ show xIO ++ "," ++ show yIO
--        ++ "\n xB,yB = " ++ show xB ++ "," ++ show yB
--        ++ "\n xC,yC = " ++ show xC ++ "," ++ show yC
--    ) $
    (xC,yC)
    where
    -- convert from unit square coordinates to canvas coordinates,
    -- mapping the unit square to the centre one of nine equal squares:
    -- ----------
    -- |  |  |  | 
    -- ----------
    -- |  |XX|  | 
    -- ----------
    -- |  |  |  | 
    -- ----------
    
    xC = w*(1/2 + xL/3)
    yC = h*(1/2 - yL/3) -- cairo's origin is the top left corner
    -- convert relative coords from IRA to Double:
    [(xL,xR), (yL,yR)] = map RA.doubleBounds [xU, yU]
    -- translate box coords to unit square coord relative to initbox:
    xU = (xB - xIO) / wI
    yU = (yB - yIO) / hI
    -- extract initbox origin and half-width and half-height:
    ([xIO,yIO],wI,hI) = initboxInfo


