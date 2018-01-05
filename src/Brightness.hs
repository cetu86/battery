import System.Environment (getArgs)
import FileTools

data Mode = Inc | Dec deriving (Show,Read,Eq)

readMode :: String -> Maybe Mode
readMode "inc" = Just Inc
readMode "dec" = Just Dec
readMode _ = Nothing

brightnessDir = "/sys/class/backlight/intel_backlight/"
maxBrightnessFilePath = brightnessDir ++ "max_brightness"
brightnessFilePath = brightnessDir ++ "brightness"

data BrightnessInfo = BrightnessInfo {
  currentBrightnessPercent :: Int,
  maxBrightness :: Int
}


readBrightness :: IO BrightnessInfo
readBrightness = do
  maxBrightness <- readIntFromFileOr maxBrightnessFilePath 0 
  brightness <- readIntFromFileOr brightnessFilePath 0 
  return $ BrightnessInfo (round $ (fromIntegral brightness / fromIntegral maxBrightness ) * 100 ) maxBrightness

clip lowerLimit upperLimit = max lowerLimit . min upperLimit 

changeBrightness :: (Int -> Int) -> IO ()
changeBrightness f = do
  brightnessInfo <- readBrightness
  let curprcnt = currentBrightnessPercent brightnessInfo
  let cur = fromIntegral (f curprcnt) / 100
  let mx = maxBrightness brightnessInfo
  let newBrightness = clip 0 mx $ round (cur * fromIntegral mx)
  writeIntToFile brightnessFilePath newBrightness


main = do
  args <- getArgs
  case args of
    [] -> readBrightness >>= (print . currentBrightnessPercent)
    [x] -> case readMode x of
            Nothing -> print "hä?"
            Just Inc -> changeBrightness (+ 2)
            Just Dec -> changeBrightness (\x -> x - 2)
