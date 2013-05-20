
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

import Graphics.UI.Gtk hiding (drawPolygon)
import Graphics.Rendering.Cairo

import Control.Concurrent
import Control.Concurrent.STM

--import qualified Data.Map as Map
import qualified Data.IntMap as IMap

{-|
  sample usage: @addBox stateTV (1,0,0,0.9) box@
-}
addBox stateTV rgba box  
    =
    do
--    putStrLn $ "addBox called" -- ++ ppShow box
    atomically $ do
        state <- readTVar stateTV
        writeTVar stateTV ((box,rgba) : state)
--    putStrLn $ "addBox completed"

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
    [p1@[x1,y1], p4@[_,y4], p2@[x2,_], p3] = ppCorners affines
    (_isSkewed, affines, _varIsInts, varNamesMap) = initbox
    varNames = map snd $ IMap.toAscList varNamesMap
    initboxInfo = (ppCentre affines, x2 - x1, y4 - y1, varNames)

myCanvas stateTV draw initboxInfo w h = 
    do
--    unsafeInitGUIForThreadedRTS
    initGUI
    window <- windowNew
    da <- drawingAreaNew
    set window [ containerChild := da ]
    windowSetDefaultSize window w h
    onExpose da (myExposeHandler stateTV da draw initboxInfo)
    timeoutAdd (widgetQueueDraw da >> return True) 500 >> return ()
    idleAdd (yield >> threadDelay 10000 >> return True) priorityDefaultIdle >> return ()
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
--    putStrLn $ "myExposeHandler completed"
    return True

draw initboxInfo w h state  = 
    do
    drawAxisLabels initboxInfo w h
    mapM_ (drawSubBox initboxInfo w h) state

drawAxisLabels initboxInfo w h =
    do
    setFontSize 17
    -- draw variable names:
    moveTo centreX (botLeftY + 10)
    showTextCenteredAndShiftedBy (0,0.5) var1
    moveTo (botLeftX - 10) centreY
    showTextCenteredAndShiftedBy (-0.5,0) var2
    -- draw domain endpoints on X axis:
    moveTo botLeftX (botLeftY + 10)
    showTextCenteredAndShiftedBy (0,0.5) (showAsD $ xIO - wI/2)
    moveTo topRightX (botLeftY + 10)
    showTextCenteredAndShiftedBy (0,0.5) (showAsD $ xIO + wI/2)
    -- draw domain endpoints on Y axis:
    moveTo (botLeftX - 10) botLeftY
    showTextCenteredAndShiftedBy (-0.5,0) (showAsD $ yIO - hI/2)
    moveTo (botLeftX - 10) topRightY
    showTextCenteredAndShiftedBy (-0.5,0) (showAsD $ yIO + hI/2)
    where
    (centreX, centreY) = boxCoordsToCanvasCoords initboxInfo w h (xIO,yIO)
    (botLeftX, botLeftY) = boxCoordsToCanvasCoords initboxInfo w h (xIO-wI/2,yIO-hI/2)
    (topRightX, topRightY) = boxCoordsToCanvasCoords initboxInfo w h (xIO+wI/2,yIO+hI/2)
    ([xIO,yIO],wI,hI,[var1, var2]) = initboxInfo
    showAsD aI = show aD where (_, aD) = RA.doubleBounds aI
    showTextCenteredAndShiftedBy (xShiftInWidths, yShiftInHeights) text =
        do
        extents <- textExtents text
        uncurry relMoveTo $ getPos extents
        showText text
        where
        getPos extents =
             (-w/2 +w*xShiftInWidths,h/2 + h*yShiftInHeights)
             where
             w = textExtentsWidth extents
             h = textExtentsHeight extents

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
    thickenIfLine $ map converPt [p1,p2,p3,p4]
    where
    converPt [x,y] =
        boxCoordsToCanvasCoords initboxInfo w h (x,y)
    [p1,p4,p2,p3] = ppCorners subbox

thickenIfLine orig@[(x1,y1),(x2,y2),(x3,y3),(x4,y4)] 
    | xDiff == 0 && yDiff > 2 = [(x1-1,y1),(x2+1,y2),(x3+1,y3),(x4-1,y4)]
    | yDiff == 0 && xDiff > 2 = [(x1,y1-1),(x2,y2+1),(x3,y3+1),(x4,y4-1)]
    | otherwise = orig
    where
    xDiff = sum $ map abs $ [x1-x2, x1-x3, x1-x4]
    yDiff = sum $ map abs $ [y1-y2, y1-y3, y1-y4]
-- alternative approach:    
--    check14 . check13 . check12
--    where
--    check14 [p1,p2,p3,p4] = [r1,r2,r3,r4]
--        where
--        [r1,r4,r2,r3] = check12 [p1,p4,p2,p3]
--    check13 [p1,p2,p3,p4] = [r1,r2,r3,r4]
--        where
--        [r1,r3,r2,r4] = check12 [p1,p3,p2,p4]
--    check12 orig@[p1@(x1,y1),p2,p3@(x3,y3),p4]
--        | p1 == p2 && p3 == p4 =
--            if abs (x1 - x3) < abs (y1 - y3)
--                then [(x1-1,y1),(x1+1,y1),(x3+1,y3),(x3-1,y3)]
--                else [(x1,y1-1),(x1,y1+1),(x3,y3+1),(x3,y3-1)]
--        | otherwise = orig
    

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
    -- mapping the unit square to the centre of the visible area:
    -- ----------
    -- |  |  |  | 
    -- ----------
    -- |  |XX|  | 
    -- ----------
    -- |  |  |  | 
    -- ----------
    
    xC = w*(0.5 + 0.75*xL)
    yC = h*(0.5 - 0.75*yL) -- cairo's origin is the top left corner
    -- convert relative coords from IRA to Double:
    [(xL,xR), (yL,yR)] = map RA.doubleBounds [xU, yU]
    -- translate box coords to unit square coord relative to initbox:
    xU = (xB - xIO) / (wI)
    yU = (yB - yIO) / (hI)
    -- extract initbox origin and width and height:
    ([xIO,yIO],wI,hI,_) = initboxInfo


