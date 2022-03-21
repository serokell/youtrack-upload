module Main where

import qualified Lib

main :: IO ()
main = do
  t <- Lib.commandSend "./time.csv"
  pure ()
