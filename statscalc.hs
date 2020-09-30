import Test.Hspec    

type Stats = (Int, Int, Int, Double)

stats :: [Int] -> Stats
stats x = (foldr1 min x, foldr1 max x, length x, avg x)

avg :: [Int] -> Double
avg x = (fromIntegral (sum x)) / (fromIntegral (length x))

getMin :: Stats -> Int
getMin (x,_,_,_) = x
getMax :: Stats -> Int
getMax (_,x,_,_) = x
getLength :: Stats -> Int
getLength (_,_,x,_) = x
getAverage :: Stats -> Double
getAverage (_,_,_,x) = x


-- Tests
main :: IO ()
main = hspec $ do
  describe "stats" $ do
      it "should return min, max, length and average" $ do
          -- (Min, Max, Len, Av)
        stats [6, 9, 15, -2, 92, 11] `shouldBe` (-2, 92, 6, 21.833333333333332)
      it "should return -100 as the min of list with minimum of -100" $ do
        getMin (stats [6, 9, 15, -100, 92, 11]) `shouldBe` -100
      it "should return 8 as the max of list" $ do
        getMax (stats [6, 8, -15, -100, -92, -11]) `shouldBe` 8
      it "should return a length of 2 for 2 elements in list" $ do
        getLength (stats [1, 2]) `shouldBe` 2
      it "should return an average 12 for the list [12,12,12]" $ do
        getAverage (stats [12, 12, 12]) `shouldBe` 12
      