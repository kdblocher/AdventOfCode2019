module Day1Spec (spec) where
import Day1
import Test.Hspec
import Test.QuickCheck

spec =
  describe "Day1" $ do
    describe "massToFuel" $ do
      it "For a mass of 12, divide by 3 and round down to get 4, then subtract 2 to get 2." $ do
        massToFuel 12 `shouldBe` 2
      it "For a mass of 14, dividing by 3 and rounding down still yields 4, so the fuel required is also 2." $ do
        massToFuel 14 `shouldBe` 2
      it "For a mass of 1969, the fuel required is 654." $ do
        massToFuel 1969 `shouldBe` 654
      it "For a mass of 100756, the fuel required is 33583." $ do
        massToFuel 100756 `shouldBe` 33583
    describe "massToFuel2" $ do
      it "A module of mass 14 requires 2 fuel. This fuel requires no further fuel (2 divided by 3 and rounded down is 0, which would call for a negative fuel), so the total fuel required is still just 2." $ do
        massToFuel2 14 `shouldBe` 2
      it "At first, a module of mass 1969 requires 654 fuel. Then, this fuel requires 216 more fuel (654 / 3 - 2). 216 then requires 70 more fuel, which requires 21 fuel, which requires 5 fuel, which requires no further fuel. So, the total fuel required for a module of mass 1969 is 654 + 216 + 70 + 21 + 5 = 966." $ do
        massToFuel2 1969 `shouldBe` 966
      it "The fuel required by a module of mass 100756 and its fuel is: 33583 + 11192 + 3728 + 1240 + 411 + 135 + 43 + 12 + 2 = 50346." $ do
        massToFuel2 100756 `shouldBe` 50346