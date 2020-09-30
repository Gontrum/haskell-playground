import Test.Hspec    

leapyear :: Int -> Bool
leapyear x 
    | x `mod` 400 == 0 = True
    | x `mod` 100 == 0 = False 
    | x `mod` 4 == 0 = True
    | otherwise = False

-- Tests
main :: IO ()
main = hspec $ do
  describe "leapyear" $ do
      it "should return true if given year was 400" $ do
        leapyear 400 `shouldBe` True
      it "should return true if given year was 8" $ do
        leapyear 8 `shouldBe` True
      it "should return false if given year was 300" $ do
        leapyear 300 `shouldBe` False
      it "should return false if given year was 3" $ do
        leapyear 3 `shouldBe` False
