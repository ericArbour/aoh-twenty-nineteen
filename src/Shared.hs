module Shared (getLines) where

import Control.Monad.Loops (whileM)
import System.IO
  ( FilePath
  , Handle()
  , IOMode(ReadMode)
  , hGetLine
  , hIsEOF
  , withFile
  )

checkEOF :: Handle -> IO Bool
checkEOF inputHandle = do
  isEOF <- hIsEOF inputHandle
  return $ not isEOF

getLines :: FilePath -> IO [String]
getLines filePath = do
  withFile filePath ReadMode $ \inputHandle -> do
    lines <-
      whileM (checkEOF inputHandle) $ do
        line <- hGetLine inputHandle
        return line
    return lines

