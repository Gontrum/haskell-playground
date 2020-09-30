import Test.Hspec
import Test.QuickCheck

numeralMapping = [("I",1), ("IV", 4), ("V", 5), ("IX", 9), ("X", 10), ("XL", 40), ("L", 50), ("XC", 90), ("C", 100), ("CM", 900), ("M", 1000)]

toRoman :: Int -> String
toRoman 0 = ""
toRoman arabic = replicateString howManyTimeTheRomanNumeralFitsIn romanNumberal ++ toRoman rest
  where (romanNumberal, highestNumberSmallerThanArabic) = findHighestMappingSmallerOrEqualThan arabic
        (howManyTimeTheRomanNumeralFitsIn, rest) = arabic `divMod` highestNumberSmallerThanArabic

findHighestMappingSmallerOrEqualThan :: Int -> (String, Int)
findHighestMappingSmallerOrEqualThan number = foldl1 reducer numeralMapping
  where reducer acc current  = if (snd current <= number) then current else acc

replicateString :: Int -> String -> String
replicateString times = concat . replicate times

main :: IO ()
main = hspec $ do
  describe "findHighestMappingSmallerOrEqualThan" $ do 
    it "should return (XL, 40) for 43" $ do
      findHighestMappingSmallerOrEqualThan 43 `shouldBe` ("XL", 40)
  describe "toRoman" $ do
    let specs = map makeSpec
            [ ("I", 1),
              ("II", 2),
              ("III", 3),
              ("IV", 4),
              ("V", 5),
              ("VI", 6),
              ("VII", 7),
              ("IX", 9),
              ("X", 10),
              ("XI", 11),
              ("XII", 12),
              ("XIII", 13),
              ("XIV", 14),
              ("XV", 15),
              ("XVI", 16),
              ("XIX", 19),
              ("XXIX", 29),
              ("CCCLIV", 354),
              ("MMMMCMXCIX", 4999)
            ]
        makeSpec (roman, arabic) =
          it ("should return " ++ roman ++ " for " ++ show arabic) $
            toRoman arabic `shouldBe` roman

    foldr1 (>>) specs
