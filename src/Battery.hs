module Main where

import FileTools

import Control.Monad
import Control.Concurrent

import Data.Bits

import qualified Data.ByteString as B
import Data.Word (Word8)
import Data.List (unfoldr)

import Graphics.UI.Gtk hiding (rectangle)
import Graphics.Rendering.Cairo
import Foreign.Ptr (castPtr)

import Debug.Trace

import Utils


displayStatus :: (Int,Bool) -> IO Pixbuf
displayStatus (chargePercent,charging) = renderPixbuf 16 $ do 
    let chargeFraction = realToFrac chargePercent / 100
    let h  = chargeFraction * 16
    setLineWidth 1.5
    setSourceOnGradient redish greenish chargeFraction
    rectangle 1 (16-h) 8 h
    fill
    setSourceHex greyish
    rectangle 1 0 8 16
    stroke
    when charging $ do
      setLineWidth 2
      setSourceHex yellowish
      moveTo 12 1
      lineTo 15 8.5
      lineTo 12 8.5
      lineTo 15 15
      stroke


bat x = "/sys/class/power_supply/BAT0/" ++ x

data DemoMode = DemoOff | DemoOn Bool Int



toggleDemoMode m = modifyMVar_ m $ return . toggleDemoMode'
  where toggleDemoMode' DemoOff = DemoOn False 0
        toggleDemoMode' (DemoOn _ _) = DemoOff

getDelay m = getDelay' <$> readMVar m
  where getDelay' DemoOff = 1000000
        getDelay' (DemoOn _ _) = 20000

getStatus :: MVar DemoMode -> IO (Int,Bool)
getStatus m = do 
  mode <- takeMVar m
  case mode of
    DemoOff -> putMVar m mode >> getRealStatus 
    DemoOn charging chargePercent -> do
      let (charging',chargePercent') = if chargePercent < 100 
            then (charging,chargePercent + 1)
            else (not charging,0)
      putMVar m $ DemoOn charging' chargePercent'
      return (chargePercent',charging')


getRealStatus = do
  charge <- realToFrac <$> readIntFromFileOr (bat "charge_now") 0
  fullCap <- realToFrac <$> readIntFromFileOr (bat "charge_full") 1
  let chargePercent = round $ 100 * charge / fullCap
  charging <- (/= "Discharging\n") <$> readFileOrEmpty (bat "status")
  return (chargePercent,charging)


main = do
  initGUI

  let undetermined = (0,False)

  mode <- newMVar DemoOff

  let readMode = readMVar mode

  icon <- displayStatus undetermined >>= statusIconNewFromPixbuf
  statusIconSetVisible icon True

  let loop lastStatus = do
        status <- getStatus mode
        when (status /= lastStatus) $ postGUIAsync $ do 
          pixbuf <- displayStatus status
          set icon [ 
              statusIconPixbuf := pixbuf,
              statusIconTooltipText := Just (show (fst status) ++ " %")
            ]
        getDelay mode >>= threadDelay
        loop status

  forkIO $ loop undetermined


  menu <- menuNew
  let mkitem (label,act) = do 
          item <- menuItemNewWithLabel label
          menu `menuShellAppend` item
          item `on` menuItemActivated $ act
  mapM_ mkitem [ ("DemoMode On/Off",toggleDemoMode mode),("Quit",mainQuit) ]

  icon `on` statusIconPopupMenu $ \b a -> do
         widgetShowAll menu
         menuPopup menu $ maybe Nothing (\b' -> Just (b',a)) b

  mainGUI
