import Day1Spec
import Day2Spec
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    Day1Spec.spec
    Day2Spec.spec