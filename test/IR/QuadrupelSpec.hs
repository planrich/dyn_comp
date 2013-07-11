module IR.QuadrupelSpec where


import Test.Hspec

spec :: Spec
spec = do
    describe "quadrupel" $ do
        it "1 + 1 is 2" (((+) 1 1) `shouldBe` 2)

