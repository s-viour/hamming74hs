module Main where

import Lib
import System.Environment
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1 then
    putStrLn ("wrong number of arguments\n" ++ usage)
  else if head args == "encode" then
    BL.getContents >>= BL.putStr . encode
  else if head args == "decode" then
    BL.getContents >>= BL.putStr . decode
  else
    putStrLn ("unknown argument: " ++ head args ++ "\n" ++ usage)

usage :: String
usage = "hamming74hs encode|decode"