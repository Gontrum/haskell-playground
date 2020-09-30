import Test.Hspec
import Data.List (permutations, sort)  

anagram :: String -> [String]
anagram [char] = [[char]]
anagram x = sort (foldr reducer [] x)
    where reducer char strings = strings ++ map (char:) (anagram (x `without` char))

without :: String -> Char -> String
without [] toremove = []
without (first:rest) toremove 
    | first == toremove         = rest
    | otherwise                 = first:(rest `without` toremove)

-- Tests
main :: IO ()
main = hspec $ do
    describe "anagram" $ do
        it "should return a for a" $ do
            anagram "a" `shouldBe` ["a"]
        it "should return ab and ba for ab" $ do
            anagram "ab" `shouldBe` ["ab", "ba"]
        it "should return ac and ca for ac" $ do
            anagram "ac" `shouldBe` ["ac", "ca"]
        it "should return ad and da for ad" $ do
            anagram "ad" `shouldBe` ["ad", "da"]
        it "should return anagrams for abc" $ do
            anagram "abc" `shouldBe` ["abc", "acb", "bac", "bca", "cab", "cba"]
        it "should return anagrams for 123" $ do
            anagram "123" `shouldBe` ["123", "132", "213", "231", "312", "321"]
        it "should return anagrams for abcd" $ do
            anagram "abcd" `shouldBe` (sort . permutations) "abcd"

    describe "without" $ do 
        it "should remove first a from anna" $ do
            "anna" `without` 'a' `shouldBe` "nna"



-- anagram :: String -> [String]
-- anagram x
--     | length x == 1 = [x]
--     | length x == 3 = sort (anagramReduce x)
--     | length x == 4 = sort (anagramReduce x)
--     | otherwise = [x, reverse x]

-- anagram3 :: String -> [String]
-- anagram3 (a:b:c:_) = [a:b:c:[], a:c:b:[], b:a:c:[], b:c:a:[], c:a:b:[], c:b:a:[]]