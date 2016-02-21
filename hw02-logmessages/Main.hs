module Main where

import LogAnalysis
import Log


main :: IO [String]
main =
  testWhatWentWrong parseFile whatWentWrong "error.log"
