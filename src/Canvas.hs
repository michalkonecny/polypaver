
module Canvas (canvas,draw) where

import Control.Concurrent
import Control.Concurrent.STM

import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import qualified Numeric.ER.Real.Approx as RA

import Graphics.Rendering.Cairo
--import Data.Time.Clock.POSIX
--import Graphics.Cairo.
import Graphics.UI.Gtk.General.General
import Graphics.UI.Gtk.Windows.Window
import Graphics.UI.Gtk.Misc.DrawingArea
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Container
import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Cairo

import System.CPUTime

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

canvas inittime currtimeTV stateTV draw w h = do
    unsafeInitGUIForThreadedRTS
    window <- windowNew
    da <- drawingAreaNew
    currtime <- atomically $ readTVar currtimeTV
    set window 
      [containerChild := da, 
       windowTitle := "After 0 seconds"]
    windowSetDefaultSize window w h
    onExpose da (exposeHandler inittime currtimeTV stateTV da window draw)
    timeoutAdd (widgetQueueDraw da >> return True) 1000 >> return ()
    idleAdd (yield >> return True) priorityDefaultIdle >> return ()
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI

exposeHandler inittime currtimeTV stateTV widget window draw e = do
    currtime <- atomically $ readTVar currtimeTV
    set window [windowTitle := "After " ++ show
                      (-(floor(-((fromInteger (currtime-inittime)) / 1000000000000))))
                      ++ " seconds"]
    drawWin <- widgetGetDrawWindow widget
    (wi,hi) <- widgetGetSize widget
    let (w,h) = (realToFrac wi, realToFrac hi)
    state <- atomically $ readTVar stateTV
    renderWithDrawable drawWin $ do
        draw w h state -- (map realToFrac state)
    return True
