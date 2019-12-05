module Day1Spec (spec) where
import Day1
import Test.Hspec
import Test.QuickCheck

spec =
  describe "Day1.massToFuel" $ do
    it "For a mass of 12, divide by 3 and round down to get 4, then subtract 2 to get 2." $ do
      massToFuel 12 `shouldBe` 2
    it "For a mass of 14, dividing by 3 and rounding down still yields 4, so the fuel required is also 2." $ do
      massToFuel 14 `shouldBe` 2
    it "For a mass of 1969, the fuel required is 654." $ do
      massToFuel 1969 `shouldBe` 654
    it "For a mass of 100756, the fuel required is 33583." $ do
      massToFuel 100756 `shouldBe` 33583