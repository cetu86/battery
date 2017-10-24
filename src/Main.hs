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


splitAtIfNotNull :: Int -> B.ByteString -> Maybe (B.ByteString,B.ByteString)
splitAtIfNotNull i string 
  | B.null string = Nothing
  | otherwise = Just $ B.splitAt i string 

mapChunks :: (B.ByteString -> B.ByteString) -> Int -> B.ByteString -> B.ByteString
mapChunks mapper chunkSize = B.concat . map mapper . unfoldr (splitAtIfNotNull chunkSize)

swapRB :: Int -> B.ByteString -> B.ByteString
swapRB = mapChunks swapRBforLine
  where swapRBforLine :: B.ByteString -> B.ByteString
        swapRBforLine = mapChunks (B.pack . swapRBforPixel . B.unpack) 4 
        swapRBforPixel :: [Word8] -> [Word8]
        swapRBforPixel [b,g,r,a] = [r,g,b,a]
        swapRBforPixel other = other

renderPixbuf :: Int -> Render a -> IO Pixbuf
renderPixbuf size render = withImageSurface FormatARGB32 size size $ \surface -> do
    renderWith surface render
    stride <- imageSurfaceGetStride surface
    surfaceData <- swapRB stride <$> imageSurfaceGetData surface
    B.useAsCStringLen surfaceData $ \(pointer, _) -> 
      let pointer' = castPtr pointer
      in pixbufNewFromData pointer' ColorspaceRgb True 8 size size stride

byteToFloat x = fromIntegral x / 255

getBytesFromHex :: Int -> [Int]
getBytesFromHex = intToByteList 3

getChannelsFromHex :: Int -> [Double]
getChannelsFromHex = fmap byteToFloat . getBytesFromHex

intToByteList :: Int -> Int -> [Int]
intToByteList nBytes value = map extractByte $ inverseRange nBytes
  where extractByte i = (value .&. (0xff `shift` (8 * i))) `shift` ((-8) * i)
        inverseRange n = [n-1,n-2..0]

setSourceHex :: Int -> Render ()
setSourceHex value = setSourceRGB r g b
  where [r,g,b] = getChannelsFromHex value

dmod :: Double -> Double -> Double
dmod x m = x - fromIntegral ( floor (x / m) ) * m

testconv :: (Int,Int,Int) -> (Int,Int,Int)
testconv = (\(a,b,c) -> (i a,i b,i c)). hsv2rgb . rgb2hsv
  where i x = round $ x * 255

range = [(a,b,c) | a <- r, b <- r, c <- r]
 where r = [0..255]

testall = filter (\x -> testconv x /= x) range
 

hsv2rgb :: (Double,Double,Double) -> (Double,Double,Double)
hsv2rgb (h,s,v) = 
  let c = s * v
      sector = (floor (h / 60) `mod` 6) :: Int
      x = c * ( 1 - abs ( ((h / 60) `dmod` 2)- 1 ) )
      
      m = v - c
      (r',g',b') =  [
        (c,x,0),
        (x,c,0),
        (0,c,x),
        (0,x,c),
        (x,0,c),
        (c,0,x)
       ] !! sector
      a x = x + m
  in (a r',a g',a b')


rgb2hsv :: (Int,Int,Int) -> (Double,Double,Double)
rgb2hsv (r,g,b) = 
  let minRGB = min r $ min g b
      maxRGB = max r $ max g b
  in if minRGB == maxRGB
      then (0,0,f minRGB)
      else let select 
                | r == minRGB = 0
                | b == minRGB = 1
                | otherwise = 2
               d = [g-b,r-g,b-r] !! select
               sector = [3,1,5] !! select
               mm = maxRGB - minRGB

               h = 60*(sector - f d / f mm)
               s = f mm / f maxRGB;
               v = f maxRGB
            in (h,s,v)
 where f = byteToFloat

getHSVFromHex :: Int -> (Double,Double,Double)
getHSVFromHex v =
  let [r,g,b] = getBytesFromHex v
  in rgb2hsv(r,g,b)

setSourceOnGradient :: Int -> Int -> Double -> Render ()
setSourceOnGradient start end fraction =
  let (hs,ss,vs) = getHSVFromHex start
      (he,se,ve) = getHSVFromHex end
      interpolate a b = ( b - a) * fraction + a
      h 
        | hs - he > 180 = interpolate hs (he + 360)
        | he - hs > 180 = interpolate (hs + 360) he
        | otherwise = interpolate hs he
      s = interpolate ss se
      v = interpolate vs ve
      (r,g,b) = hsv2rgb (h,s,v)
  in setSourceRGB r g b

greenish = 0x00e410
yellowish = 0xfffc00
redish = 0xff002c 
greyish = 0xdcdccc

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
