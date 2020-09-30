import Test.Hspec    
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
-- Tests
main :: IO ()
main = hspec $ do
  describe "fib returning elements of the fibonacci sequence" $ do
    it "returns the 0th element" $ do
        fib 0 `shouldBe` 0