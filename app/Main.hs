module Main where

import Day1
import Day2
import Control.Applicative
import Control.Monad
import Flow

run :: Part -> FilePath -> IO ()
run part =
  readFile
  >=> process part .> return
  >=> putStrLn

main :: IO ()
main =
     liftA2 (>>) (run part1) (run part2) "input/day1.txt"
  >> 
 