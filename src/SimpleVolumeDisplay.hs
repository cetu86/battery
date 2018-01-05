module Main where

import FileTools
import System.Process

import Control.Monad
import Control.Concurrent

import Data.Bits

import qualified Data.ByteString as B
import Data.Word (Word8)
import Data.List (unfoldr)

import Graphics.UI.Gtk hiding (rectangle)
import Graphics.Rendering.Cairo
import Foreign.Ptr (castPtr)

import System.IO.Error

import Debug.Trace

import Utils


displayStatus :: (Int,Bool) -> IO Pixbuf
displayStatus (volumePercent,muted) = renderPixbuf 16 $ do 
    let volumeFraction = realToFrac volumePercent / 100
    setLineWidth 1.5
    setSourceHex 0x000000
    arc 8.5 8.5 8 0 ( 2 * pi)
    fill
    moveTo 8.5 8.5
    if muted
      then setSourceHex 0xaaaaaa
      else setSourceHex 0xfcfcf0

    arc 8.5 8.5 8 (pi / 2) ( pi / 2 + (volumeFraction * 2 * pi))
    moveTo 8.5 8.5
    closePath
    fill
    setSourceHex 0x000000
    arc 8.5 8.5 4 0 ( 2 * pi)
    fill

    if muted
      then do
        setLineWidth 2
        setSourceHex redish
        arc 8.5 8.5 2.5 0 (2 * pi)
        moveTo 6 8.5
        lineTo 11 8.5
        stroke
      else do
        setLineWidth 2
        setSourceHex greenish
        arc 8.5 8.5 2.5 0 (2 * pi)
        stroke


getStatus :: IO (Int,Bool)
getStatus = catchIOError readStatus quitOnError
 where readStatus = parseLine . words <$> getLine

       parseLine [] = undetermined
       parseLine (volume:[]) = (readIntFromStringOr volume 0,False)
       parseLine (volume:mute) = (readIntFromStringOr volume 0,True)

       quitOnError = const $ postGUIAsync mainQuit >> return undetermined

       undetermined = (0,False)

spawnPavucontrol :: IO ()
spawnPavucontrol = do
  void $ forkIO $ callCommand "pavucontrol"

main = do
  initGUI

  initialStatus <- getStatus

  icon <- displayStatus initialStatus >>= statusIconNewFromPixbuf
  statusIconSetVisible icon True

  let loop lastStatus = do
        status <- getStatus
        when (status /= lastStatus) $ postGUIAsync $ do 
          pixbuf <- displayStatus status
          set icon [ 
              statusIconPixbuf := pixbuf,
              statusIconTooltipText := Just (show (fst status) ++ " %")
            ]
        loop status

  forkIO $ loop initialStatus

  menu <- menuNew
  let mkitem (label,act) = do 
          item <- menuItemNewWithLabel label
          menu `menuShellAppend` item
          item `on` menuItemActivated $ act
  mapM_ mkitem [ ("mixer",spawnPavucontrol),("Quit",mainQuit) ]

  icon `on` statusIconPopupMenu $ \b a -> do
         widgetShowAll menu
         menuPopup menu $ maybe Nothing (\b' -> Just (b',a)) b

  icon `on` statusIconActivated $ spawnPavucontrol


  mainGUI
