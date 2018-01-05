module FileTools where

import Control.Exception
import System.IO
import qualified System.IO.Strict as SIO

readFileOrEmpty :: String -> IO String
readFileOrEmpty file = catch (SIO.readFile file) handleIOException
  where handleIOException :: IOException -> IO String
        handleIOException = const $ return []

readIntFromStringOr :: String -> Int -> Int
readIntFromStringOr string value = do
  let readValue = reads string :: [(Int,String)]
  if null readValue
    then value
    else fst $ head readValue

readIntFromFileOr :: String -> Int -> IO Int
readIntFromFileOr file value = do
  string <- readFileOrEmpty file
  return $ readIntFromStringOr string value

writeIntToFile :: String -> Int->IO ()
writeIntToFile file = writeFile file . show
  
