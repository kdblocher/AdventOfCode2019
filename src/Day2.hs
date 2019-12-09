{-# LANGUAGE ScopedTypeVariables #-}
module Day2 (run) where
import Core
import Flow
import Control.Monad
import Control.Monad.Loops
import Control.Monad.ST
import Data.Array.Unboxed
import Data.Array.ST

{-
On the way to your gravity assist around the Moon, your ship computer beeps angrily about a "1202 program alarm". On the radio, an Elf is already explaining how to handle the situation: "Don't worry, that's perfectly norma--" The ship computer bursts into flames.

You notify the Elves that the computer's magic smoke seems to have escaped. "That computer ran Intcode programs like the gravity assist program it was working on; surely there are enough spare parts up there to build a new Intcode computer!"

An Intcode program is a list of integers separated by commas (like 1,0,0,3,99). To run one, start by looking at the first integer (called position 0). Here, you will find an opcode - either 1, 2, or 99. The opcode indicates what to do; for example, 99 means that the program is finished and should immediately halt. Encountering an unknown opcode means something went wrong.

Opcode 1 adds together numbers read from two positions and stores the result in a third position. The three integers immediately after the opcode tell you these three positions - the first two indicate the positions from which you should read the input values, and the third indicates the position at which the output should be stored.

For example, if your Intcode computer encounters 1,10,20,30, it should read the values at positions 10 and 20, add those values, and then overwrite the value at position 30 with their sum.

Opcode 2 works exactly like opcode 1, except it multiplies the two inputs instead of adding them. Again, the three integers after the opcode indicate where the inputs and outputs are, not their values.

Once you're done processing an opcode, move to the next one by stepping forward 4 positions.
-}

type Intcode = [Int]
type IntcodeArray s = STUArray s Int Int
type IntcodeST s = ST s (IntcodeArray s)
type Counter = Int
type Program s = (Counter, IntcodeST s)

data Opcode = Add | Multiply | Halt
getOpcode :: Int -> Opcode
getOpcode 1  = Add
getOpcode 2  = Multiply
getOpcode 99 = Halt
getOpcode _  = error "Bad opcode"

executeOnce :: Counter -> IntcodeST s -> IntcodeST s
executeOnce counter m = do
  code <- m
  opcode <- readArray code counter
  case getOpcode opcode of
    Halt     -> return code
    Add      -> step code (+)
    Multiply -> step code (*)
  where
    step :: IntcodeArray s -> (Int -> Int -> Int) -> IntcodeST s
    step code op = do
      let read = readArray code
          write = writeArray code
      in1 <- read $ counter + 1
      in2 <- read $ counter + 2
      out <- read $ counter + 3
      arg1 <- read in1
      arg2 <- read in2
      write out $ arg1 `op` arg2
      return code

execute :: IntcodeST s -> IntcodeST s
execute m = f 0 m
  where
    f :: Counter -> IntcodeST s -> IntcodeST s
    f counter m = do
      code <- m
      val <- readArray code counter
      let x = getOpcode val
      case x of
        Halt -> m
        _    -> f (counter + 4) (executeOnce counter m)

run :: Intcode -> Intcode
run ls = elems <| runSTUArray $ do
  execute $ newListArray (0, length ls - 1) ls :: IntcodeST s 