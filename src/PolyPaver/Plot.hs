
module PolyPaver.Plot 
(
    initPlot,
    addBox,
    State,
--    ,
--    waitForClose
)
where

import PolyPaver.APBox

import qualified Numeric.ER.Real.Approx as RA
import Numeric.ER.Real.Base
import Numeric.ER.Real.DefaultRepr
--import Numeric.ER.Misc

import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp((:=)))
import qualified Graphics.Rendering.Cairo as Cairo

import Control.Concurrent (forkIO, yield, threadDelay)
import Control.Concurrent.STM (TVar, writeTVar, readTVar, newTVar, atomically)

--import qualified Data.Map as Map
import qualified Data.IntMap as IMap

type RGBA = (Double, Double, Double, Double)
type State b = [(APBox b, RGBA)]

{-|
  sample usage: @addBox stateTV (1,0,0,0.9) box@
-}
addBox :: TVar (State b) -> RGBA -> APBox b -> IO ()
addBox stateTV rgba box
    =
    do
--    putStrLn $ "addBox called" -- ++ showBox box
    atomically $ do
        state <- readTVar stateTV
        writeTVar stateTV ((box,rgba) : state)
--    putStrLn $ "addBox completed"


initPlot :: 
    ERRealBase b =>
    APBox b -> 
    Int -> 
    Int -> 
    IO (TVar (State b))
initPlot initbox w h =
    do
    stateTV <- atomically $ newTVar []
    _ <- forkIO $ myCanvasThread stateTV initboxInfo w h
    return stateTV
    where
    -- 4---3
    -- |   |
    -- 1---2
    [_p1@[x1,y1], _p4@[_,y4], _p2@[x2,_], _p3] = boxCorners initbox
    (APBox _box _varIsInts varNamesMap) = initbox
    varNames = map snd $ IMap.toAscList varNamesMap
    initboxInfo = (boxCentre initbox, x2 - x1, y4 - y1, varNames)

myCanvasThread :: 
    (ERRealBase b) 
    =>
    TVar (State b) -> 
    ([IRA b], IRA b, IRA b, [String]) -> 
    Int -> Int -> 
    IO ()
myCanvasThread stateTV initboxInfo w h = 
    do
--    unsafeInitGUIForThreadedRTS
    _ <- Gtk.initGUI
    window <- Gtk.windowNew
    da <- Gtk.drawingAreaNew
    Gtk.set window [ Gtk.containerChild := da ]
    Gtk.windowSetDefaultSize window w h
    _ <- Gtk.onExpose da (myExposeHandler stateTV da initboxInfo)
    Gtk.timeoutAdd (Gtk.widgetQueueDraw da >> return True) 500 >> return ()
    Gtk.idleAdd (yield >> threadDelay 10000 >> return True) Gtk.priorityDefaultIdle >> return ()
    _ <- Gtk.onDestroy window Gtk.mainQuit
    Gtk.widgetShowAll window
    Gtk.mainGUI
    atomically $ writeTVar stateTV []

myExposeHandler :: 
     (Gtk.WidgetClass widget, ERRealBase b) 
     =>
     TVar (State b) -> 
     widget -> 
     ([IRA b], IRA b, IRA b, [String]) -> 
     event -> IO Bool
myExposeHandler stateTV widget initboxInfo _event = 
    do
--    putStrLn $ "myExposeHandler called"
    drawWin <- Gtk.widgetGetDrawWindow widget
    (wi,hi) <- Gtk.widgetGetSize widget
    let (w,h) = (realToFrac wi, realToFrac hi)
    state <- atomically $ readTVar stateTV
    _ <- Gtk.renderWithDrawable drawWin $ 
        do
        draw initboxInfo w h state
--    putStrLn $ "myExposeHandler completed"
    return True

draw ::
    ERRealBase b =>
    ([IRA b], IRA b, IRA b, [String]) -> 
    Double -> 
    Double -> 
    State b -> 
    Cairo.Render ()
draw initboxInfo w h state  = 
    do
    drawAxisLabels initboxInfo w h
    mapM_ (drawSubBox initboxInfo w h) state

drawAxisLabels :: 
    RA.ERIntApprox ira =>
    ([ira], ira, ira, [String]) -> 
    Double -> 
    Double -> 
    Cairo.Render ()
drawAxisLabels initboxInfo w h =
    do
    Cairo.setFontSize 17
    -- draw variable names:
    Cairo.moveTo centreX (botLeftY + 10)
    showTextCenteredAndShiftedBy (0,0.5) var1
    Cairo.moveTo (botLeftX - 10) centreY
    showTextCenteredAndShiftedBy (-0.5,0) var2
    -- draw domain endpoints on X axis:
    Cairo.moveTo botLeftX (botLeftY + 10)
    showTextCenteredAndShiftedBy (0,0.5) (showAsD $ xIO - wI/2)
    Cairo.moveTo topRightX (botLeftY + 10)
    showTextCenteredAndShiftedBy (0,0.5) (showAsD $ xIO + wI/2)
    -- draw domain endpoints on Y axis:
    Cairo.moveTo (botLeftX - 10) botLeftY
    showTextCenteredAndShiftedBy (-0.5,0) (showAsD $ yIO - hI/2)
    Cairo.moveTo (botLeftX - 10) topRightY
    showTextCenteredAndShiftedBy (-0.5,0) (showAsD $ yIO + hI/2)
    where
    (centreX, centreY) = boxCoordsToCanvasCoords initboxInfo w h (xIO,yIO)
    (botLeftX, botLeftY) = boxCoordsToCanvasCoords initboxInfo w h (xIO-wI/2,yIO-hI/2)
    (topRightX, topRightY) = boxCoordsToCanvasCoords initboxInfo w h (xIO+wI/2,yIO+hI/2)
    ([xIO,yIO],wI,hI,[var1, var2]) = initboxInfo
    showAsD aI = show aD where (_, aD) = RA.doubleBounds aI
    showTextCenteredAndShiftedBy (xShiftInWidths, yShiftInHeights) text =
        do
        extents <- Cairo.textExtents text
        uncurry Cairo.relMoveTo $ getPos extents
        Cairo.showText text
        where
        getPos extents =
             (-ww/2 +ww*xShiftInWidths,hh/2 + hh*yShiftInHeights)
             where
             ww = Cairo.textExtentsWidth extents
             hh = Cairo.textExtentsHeight extents

drawSubBox ::
    ERRealBase b =>
    ([IRA b], IRA b, IRA b, [String]) ->
    Double -> 
    Double -> 
    (APBox b, RGBA) -> 
    Cairo.Render ()
drawSubBox initboxInfo w h (subbox,(r,g,b,a)) = 
    do
--    liftIO $ putStrLn $ "drawSubBox: subbox = " ++ showBox subbox
--    liftIO $ putStrLn $ "drawSubBox: initboxInfo = " ++ show initboxInfo
    Cairo.setSourceRGBA r g b a -- 0 1 0 0.4 light green
    rectanglePath
    Cairo.fill
    Cairo.setSourceRGBA 0 0 0 1 -- black border
    Cairo.setLineWidth 0.5
    rectanglePath
    Cairo.stroke
    where
    rectanglePath =
        do
        (uncurry Cairo.moveTo) p1
        mapM_ (uncurry Cairo.lineTo) [p2,p3,p4]
        Cairo.closePath
    [p1,p2,p3,p4] = subBoxToCanvasBox initboxInfo w h subbox

{-
    Takes a box in the problem format and maps it to 
    a box in the coordinates of a canvas of the given size.
-}
subBoxToCanvasBox ::
   ERRealBase b =>
   ([IRA b], IRA b, IRA b, t3) -> 
   Double -> 
   Double -> 
   APBox b
   -> [(Double, Double)]
subBoxToCanvasBox initboxInfo w h subbox =
    thickenIfLine $ map converPt [p1,p2,p3,p4]
    where
    converPt [x,y] =
        boxCoordsToCanvasCoords initboxInfo w h (x,y)
    converPt _ = error "subBoxToCanvasBox: converPt failed"
    [p1,p4,p2,p3] = boxCorners subbox
    

thickenIfLine ::
    (Ord a, Num a) =>
    [(a, a)] -> [(a, a)]
thickenIfLine orig@[(x1,y1),(x2,y2),(x3,y3),(x4,y4)] 
    | xDiff == 0 && yDiff > 2 = [(x1-2,y1),(x2+2,y2),(x3+2,y3),(x4-2,y4)]
    | yDiff == 0 && xDiff > 2 = [(x1,y1-2),(x2,y2+2),(x3,y3+2),(x4,y4-2)]
    | otherwise = orig
    where
    xDiff = sum $ map abs $ [x1-x2, x1-x3, x1-x4]
    yDiff = sum $ map abs $ [y1-y2, y1-y3, y1-y4]
thickenIfLine _ = error "thickenIfLine applied to incorrect argument"

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
boxCoordsToCanvasCoords ::
    RA.ERIntApprox t =>
    ([t], t, t, t1) -> Double -> Double -> 
    (t, t) -> (Double, Double)
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
    
    xC = w*(0.5 + 0.35*xL)
    yC = h*(0.5 - 0.35*yL) -- cairo's origin is the top left corner
    -- convert relative coords from IRA to Double:
    [(xL,_xR), (yL,_yR)] = map RA.doubleBounds [xU, yU]
    -- translate box coords to unit square coord relative to initbox:
    xU = (xB - xIO) / (wI)
    yU = (yB - yIO) / (hI)
    -- extract initbox origin and width and height:
    ([xIO,yIO],wI,hI,_) = initboxInfo


