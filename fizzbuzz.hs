import Test.Hspec    

fizzbuzz :: Int -> String
fizzbuzz x
    | x `mod` 15 == 0 = "fizzbuzz"
    | x `mod` 3 == 0 = "fizz"
    | x `mod` 5 == 0 = "buzz"
    | otherwise  = show x

-- Tests
main :: IO ()
main = hspec $ do
  describe "fizzbuzz" $ do
    it "should return 1 for 1" $ do
        fizzbuzz 1 `shouldBe` "1"
    it "should return 2 for 2" $ do
        fizzbuzz 2 `shouldBe` "2"
    it "should return fizz for 3" $ do 
        fizzbuzz 3 `shouldBe` "fizz"
    it "should return 4 for 4" $ do
        fizzbuzz 4 `shouldBe` "4"
    it "should return buzz for 5" $ do
        fizzbuzz 5 `shouldBe` "buzz"
    it "should return fizz for 6" $ do
        fizzbuzz 6 `shouldBe` "fizz"
    it "should return fizzbuzz for 15" $ do
        fizzbuzz 15 `shouldBe` "fizzbuzz"
    it "should return fizzbuzz for 30" $ do
        fizzbuzz 30 `shouldBe` "fizzbuzz"