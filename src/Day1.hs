module Day1 (massToFuel, processPart1) where
import Core
import Data.Int
import Data.Foldable
import Flow

{-
The Elves quickly load you into a spacecraft and prepare to launch.

At the first Go / No Go poll, every Elf is Go until the Fuel Counter-Upper. They haven't determined the amount of fuel required yet.

Fuel required to launch a given module is based on its mass. Specifically, to find the fuel required for a module, take its mass, divide by three, round down, and subtract 2.
-}

type Fuel = Int
type Mass = Int

massToFuel :: Mass -> Fuel
massToFuel m = m `div` 3 - 2

{-
The Fuel Counter-Upper needs to know the total fuel requirement. To find it, individually calculate the fuel needed for the mass of each module (your puzzle input), then add together all the fuel values.

What is the sum of the fuel requirements for all of the modules on your spacecraft?
-}

part1 :: [Mass] -> Fuel
part1 = sum . map massToFuel

processPart1 :: PartProcessor
processPart1 =
  lines
  .> map readInt
  .> part1
  .> show
  where readInt = read :: (String -> Int)