module Day2Spec (spec) where
import Day2
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Modifiers

spec =
  describe "Day2" $ do
    describe "run" $ do
      it "[99] should just halt" $ do
        run [99] `shouldBe` [99]
      -- it "Any program first halting" $
      --   let p (NonEmpty xs) = (head xs == 99) ==> run xs == xs
      --   in property p
      it "1,9,10,3,2,3,11,0,99,30,40,50 -> 3500,9,10,70,2,3,11,0,99,30,40,50 - iterates twice, then halts" $ do
        run [1,9,10,3,2,3,11,0,99,30,40,50] `shouldBe` [3500,9,10,70,2,3,11,0,99,30,40,50]
      it "1,0,0,0,99 -> 2,0,0,0,99 (1 + 1 = 2)" $ do
        run [1,0,0,0,99] `shouldBe` [2,0,0,0,99]
      it "2,3,0,3,99 -> 2,3,0,6,99 (3 * 2 = 6)" $ do
        run [2,3,0,3,99] `shouldBe` [2,3,0,6,99]
      it "2,4,4,5,99,0 -> 2,4,4,5,99,9801 (99 * 99 = 9801)" $ do
        run [2,4,4,5,99,0] `shouldBe` [2,4,4,5,99,9801]
      it "1,1,1,4,99,5,6,0,99 -> 30,1,1,4,2,5,6,0,99" $ do
        run [1,1,1,4,99,5,6,0,99] `shouldBe` [30,1,1,4,2,5,6,0,99]
          