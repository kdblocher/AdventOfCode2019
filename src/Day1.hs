module Day1 (massToFuel, massToFuel2, part1, part2, process, Part) where
import Core
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

type Part = Mass -> Fuel

process :: Part -> PartProcessor
process part =
  lines
  .> map readInt
  .> (sum . map part)
  .> show
  where readInt = read :: (String -> Int)

part1 :: Part
part1 = massToFuel

{-
During the second Go / No Go poll, the Elf in charge of the Rocket Equation Double-Checker stops the launch sequence. Apparently, you forgot to include additional fuel for the fuel you just added.

Fuel itself requires fuel just like a module - take its mass, divide by three, round down, and subtract 2. However, that fuel also requires fuel, and that fuel requires fuel, and so on. Any mass that would require negative fuel should instead be treated as if it requires zero fuel; the remaining mass, if any, is instead handled by wishing really hard, which has no mass and is outside the scope of this calculation.

So, for each module mass, calculate its fuel and add it to the total. Then, treat the fuel amount you just calculated as the input mass and repeat the process, continuing until a fuel requirement is zero or negative.
-}

massToFuel2 :: Mass -> Fuel
massToFuel2 m
  | m < 9     = 0
  | otherwise =
    let result = massToFuel m
    in result + massToFuel2 result

{-
What is the sum of the fuel requirements for all of the modules on your spacecraft when also taking into account the mass of the added fuel? (Calculate the fuel requirements for each module separately, then add them all up at the end.)
-}

part2 :: Part
part2 = massToFuel2