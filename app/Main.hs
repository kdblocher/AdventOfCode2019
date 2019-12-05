module Main where

import Day1
import Control.Monad
import Flow

run :: FilePath -> IO ()
run =
  readFile
  >=> processPart1 .> return
  >=> putStrLn

main :: IO ()
main = run "input/day1.txt"