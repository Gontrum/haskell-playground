import Data.Time (Day, fromGregorian)
import Data.List (intersect)
import Data.Function ((&))
import Data.List (sort) 
import Data.Map (filterWithKey, keys, elems, empty, singleton, lookup, insert, fromList, Map, mapWithKey)
import qualified Data.Map (map, filter)
import Test.Hspec

type Name = String
type DaysSupply = Int
type DispenseDate = Day

data Prescription = Prescription DispenseDate DaysSupply

data Medicine = Medicine Name [Prescription]

data Patient = Patient [Medicine]

prescriptionForToday :: Prescription
prescriptionForToday = Prescription (fromGregorian 2020 9 29) 30

addMedicine :: Patient -> Medicine -> Patient
addMedicine (Patient oldMedicine) medicine = Patient $ medicine:oldMedicine

addPrescription :: Medicine -> Prescription -> Medicine
addPrescription (Medicine name oldPrescriptions) prescription = Medicine name (prescription:oldPrescriptions)

getDays :: Medicine -> [DispenseDate]
getDays medicines = map (\(Prescription day _) -> day) (getPrescriptions medicines)
    where getPrescriptions (Medicine name prescriptions) = prescriptions

clash :: Patient -> [Name] -> [Day]
clash (Patient meds) names = meds
    & map getDays
    & concat
    & filter (\day -> allMedicinesAreTakenOn day meds names)
    & removeDuplicates

allMedicinesAreTakenOn :: Day -> [Medicine] -> [Name] -> Bool
allMedicinesAreTakenOn day meds names =  names `subset` namesOfMedicinesTakenOn day
    where   namesOfMedicinesTakenOn day = keys $ medicinesTakenOn day $ mapNamesToDays meds
medicinesTakenOn :: Day -> Map String [Day] -> Map String [Day]
medicinesTakenOn day = Data.Map.filter (\days -> day `elem` days)

mapNamesToDays :: [Medicine] -> Map String [Day]
mapNamesToDays = foldl mapNamesToDaysReducer empty
mapNamesToDaysReducer :: Map Name [Day] -> Medicine -> Map Name [Day]
mapNamesToDaysReducer acc (Medicine name prescriptions) = insert name mergedDays acc
        where   mergedDays = removeDuplicates $ prescriptionDays ++ existingNames :: [Day]
                prescriptionDays = map (\(Prescription days _) -> days) prescriptions :: [Day]
                existingNames = orEmptyList lookupNames :: [Day]
                lookupNames = Data.Map.lookup name acc :: Maybe [Day]
                orEmptyList (Just content) = content :: [Day]
                orEmptyList Nothing = [] :: [Day]

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldl (\acc curr -> if curr `elem` acc then acc else curr:acc) []

subset :: (Foldable t, Eq a) => [a] -> t a -> Bool
subset a b = null [x | x <- a , (x `elem` b) == False]


-- Tests
main :: IO ()
main = hspec $ do
    let penicillin prescription = Medicine "penicillin" [prescription]
        aspirin prescription = Medicine "aspirin" [prescription]
        prescription_January_First = Prescription (fromGregorian 2020 1 1) 1
        prescription_February_Second = Prescription (fromGregorian 2020 2 2) 1
        prescription_March_Third = Prescription (fromGregorian 2020 3 3) 1

    describe "medicinesTakenOn" $ do 
        it "" $ do
            keys (medicinesTakenOn (fromGregorian 2020 1 1) (mapNamesToDays [penicillin prescription_January_First, aspirin prescription_February_Second])) `shouldBe` ["penicillin"]

    describe "mapNamesToDays" $ do
        it "should create a map with a single name and day, when one medicine was given" $ do
            let medicine1 = penicillin prescription_January_First
            mapNamesToDays [medicine1] `shouldBe` (singleton "penicillin" [(fromGregorian 2020 1 1)])
        it "should create a map with two names with a day for each, when two medicines with different names where given" $ do
            let medicine1 = penicillin prescription_January_First
                medicine2 = aspirin prescription_January_First
            mapNamesToDays [medicine1, medicine2] `shouldBe` (fromList [("aspirin", [(fromGregorian 2020 1 1)]), ("penicillin", [(fromGregorian 2020 1 1 )])])
        it "should create a map with a single name with two days, when two medicines with the same name but different days were given" $ do
            let medicine1 = penicillin prescription_January_First
                medicine2 = penicillin prescription_February_Second
            mapNamesToDays [medicine1, medicine2] `shouldBe` (singleton "penicillin" [(fromGregorian 2020 1 1), (fromGregorian 2020 2 2)])
        it "should remove duplicated days in the same names" $ do
            let medicine1 = penicillin prescription_January_First
                medicine2 = penicillin prescription_January_First
            mapNamesToDays [medicine1, medicine2] `shouldBe` (singleton "penicillin" [(fromGregorian 2020 1 1)])
        it "should create a map with two medicines, one with one day and one with two days" $ do
            let medicine1 = penicillin prescription_January_First
                medicine2 = penicillin prescription_February_Second
                medicine3 = aspirin prescription_January_First
            mapNamesToDays [medicine1, medicine2, medicine3] `shouldBe` (fromList [("aspirin", [(fromGregorian 2020 1 1)]), ("penicillin", [(fromGregorian 2020 1 1), (fromGregorian 2020 2 2)])])

    describe "allMedicinesAreTakenOn" $ do 
        it "should return false when only some of the given names were taken on a given day" $ do
            let medicine1 = penicillin prescription_January_First
                medicine2 = aspirin prescription_January_First
                medicineList = [medicine1, medicine2]
                nameList = ["penicillin", "sleep"]
            allMedicinesAreTakenOn (fromGregorian 2020 1 1) medicineList nameList `shouldBe` False
        it "should return false when the medicines were found but taken on different days" $ do
            let medicine1 = penicillin prescription_January_First
                medicine2 = aspirin prescription_February_Second
                medicineList = [medicine1, medicine2]
                nameList = ["penicillin", "aspirin"]
            allMedicinesAreTakenOn (fromGregorian 2020 1 1) medicineList nameList `shouldBe` False
        it "should return true when the medicines 'penicillin' and 'aspirin' were found and taken on the same day" $ do
            let medicine1 = penicillin prescription_January_First
                medicine2 = aspirin prescription_January_First
                medicineList = [medicine1, medicine2]
                nameList = ["penicillin", "aspirin"]
            allMedicinesAreTakenOn (fromGregorian 2020 1 1) medicineList nameList `shouldBe` True
        it "should return true when the medicines 'penicillin' and 'sleep' were found and taken on the same day" $ do
            let medicine1 = penicillin prescription_January_First
                medicine2 = Medicine "sleep" [prescription_January_First]
                medicineList = [medicine1, medicine2]
                nameList = ["penicillin", "sleep"]
            allMedicinesAreTakenOn (fromGregorian 2020 1 1) medicineList nameList `shouldBe` True
        it "should return true when the medicines 'penicillin' and 'brandNewMedicine' were found and taken on the same day" $ do
            let medicine1 = penicillin prescription_January_First
                medicine2 = Medicine "brandNewMedicine" [prescription_January_First]
                medicineList = [medicine1, medicine2]
                nameList = ["penicillin", "brandNewMedicine"]
            allMedicinesAreTakenOn (fromGregorian 2020 1 1) medicineList nameList `shouldBe` True
        it "should return true when the medicines 'penicillin' and 'aspirin', but also 'brandNewMedicine' were found and taken on the same day" $ do
            let medicine1 = penicillin prescription_January_First
                medicine2 = aspirin prescription_January_First
                medicine3 = Medicine "brandNewMedicine" [prescription_January_First]
                medicineList = [medicine1, medicine2, medicine3]
                nameList = ["penicillin", "brandNewMedicine"]
            allMedicinesAreTakenOn (fromGregorian 2020 1 1) medicineList nameList `shouldBe` True
        it "should return true when the medicines 'penicillin' and 'brandNewMedicine', but also 'aspirin' were found and taken on the same day" $ do
            let medicine1 = penicillin prescription_January_First
                medicine2 = Medicine "brandNewMedicine" [prescription_January_First]
                medicine3 = aspirin prescription_January_First
                medicineList = [medicine1, medicine2, medicine3]
                nameList = ["penicillin", "brandNewMedicine"]
            allMedicinesAreTakenOn (fromGregorian 2020 1 1) medicineList nameList `shouldBe` True
        it "should return true when the medicines 'penicillin' and 'Placebo', but also 'aspirin' were found and taken on the same day" $ do
            let medicine1 = penicillin prescription_January_First
                medicine2 = Medicine "Placebo" [prescription_January_First]
                medicine3 = aspirin prescription_January_First
                medicineList = [medicine1, medicine2, medicine3]
                nameList = ["penicillin", "Placebo"]
            allMedicinesAreTakenOn (fromGregorian 2020 1 1) medicineList nameList `shouldBe` True
        it "should return true when the medicines 'penicillin' and 'aspirin' were found and taken on the same day (2.2.2020)" $ do
            let medicine1 = penicillin prescription_February_Second
                medicine2 = aspirin prescription_February_Second
                medicineList = [medicine1, medicine2]
                nameList = ["penicillin", "aspirin"]
            allMedicinesAreTakenOn (fromGregorian 2020 2 2) medicineList nameList `shouldBe` True
        it "should return true when the medicines 'penicillin' and 'aspirin' were found and taken on the same day (3.3.2020)" $ do
            let medicine1 = penicillin prescription_March_Third
                medicine2 = aspirin prescription_March_Third
                medicineList = [medicine1, medicine2]
                nameList = ["penicillin", "aspirin"]
            allMedicinesAreTakenOn (fromGregorian 2020 3 3) medicineList nameList `shouldBe` True
        it "should return false when the medicines 'penicillin' and 'brandNewMedicine' and 'aspirin' were found, but 'aspirin' was taken on a different day" $ do
            let medicine1 = penicillin prescription_January_First
                medicine2 = Medicine "brandNewMedicine" [prescription_January_First]
                medicine3 = aspirin prescription_March_Third
                medicineList = [medicine1, medicine2, medicine3]
                nameList = ["penicillin", "brandNewMedicine", "aspirin"]
            allMedicinesAreTakenOn (fromGregorian 2020 1 1) medicineList nameList `shouldBe` False
        it "should return true when the medicines 'penicillin' and 'brandNewMedicine' and 'aspirin' were found, and 'aspirin' taken on another day, too" $ do
            let medicine1 = penicillin prescription_January_First
                medicine2 = Medicine "brandNewMedicine" [prescription_January_First]
                medicine_asp_day1 = aspirin prescription_January_First
                medicine_asp_day2 = aspirin (Prescription (fromGregorian 2020 1 2) 1)
                medicineList = [medicine1, medicine2, medicine_asp_day1, medicine_asp_day2]
                nameList = ["penicillin", "brandNewMedicine", "aspirin"]
            allMedicinesAreTakenOn (fromGregorian 2020 1 1) medicineList nameList `shouldBe` True
        it "should return true when the medicines 'penicillin' and 'brandNewMedicine' and 'aspirin' were found, and 'aspirin' taken on 02.01.2020, too" $ do
            let medicine1 = penicillin prescription_January_First
                medicine2 = Medicine "brandNewMedicine" [prescription_January_First]
                medicine_asp_day1 = aspirin prescription_January_First
                medicine_asp_day2 = aspirin prescription_February_Second
                medicineList = [medicine1, medicine2, medicine_asp_day1, medicine_asp_day2]
                nameList = ["penicillin", "brandNewMedicine", "aspirin"]
            allMedicinesAreTakenOn (fromGregorian 2020 1 1) medicineList nameList `shouldBe` True
        it "should return false when a medicine with multiple dates in list was not taken on the specified date" $ do
            let penicillin1 = penicillin prescription_January_First
            let penicillin2 = penicillin prescription_February_Second
            let penicillin3 = penicillin prescription_March_Third
            allMedicinesAreTakenOn (fromGregorian 2020 4 4) [penicillin1, penicillin2, penicillin3] ["penicillin"] `shouldBe` False

    describe "medicine clash" $ do
        it "should return empty list for not clashing medicines" $ do
            let medicine1 = penicillin prescription_January_First
                medicine2 = aspirin prescription_February_Second
                patient = Patient [medicine1,medicine2]
            clash patient ["aspirin", "penicillin"] `shouldBe` []
        it "should return a clash when two medicines are taken on the same day" $ do
            let medicine1 = penicillin prescription_January_First
                medicine2 = aspirin prescription_January_First
                patient = Patient [medicine1,medicine2]
            clash patient ["aspirin", "penicillin"] `shouldBe` [fromGregorian 2020 1 1]
        it "should return no clash when only two of three medicines are taken on the same day" $ do
            let medicine1 = penicillin prescription_January_First
                medicine2 = aspirin prescription_February_Second
                medicine3 = Medicine "sleep" [prescription_February_Second]
                patient = Patient [medicine1, medicine2, medicine3]
            clash patient ["aspirin", "penicillin", "sleep"] `shouldBe` []
        it "should return two clashes when both medicines are taken own two days" $ do
            let medicine1_day1 = penicillin prescription_January_First
                medicine2_day1 = aspirin prescription_January_First
                medicine1_day2 = penicillin prescription_February_Second
                medicine2_day2 = aspirin prescription_February_Second
                patient = Patient [medicine1_day1, medicine2_day1, medicine1_day2, medicine2_day2]
            clash patient ["aspirin", "penicillin"] `shouldBe` [fromGregorian 2020 2 2, fromGregorian 2020 1 1]


-- allMedicinesAreTakenOn :: Day -> [Medicine] -> [Name] -> Bool
-- allMedicinesAreTakenOn day meds names = meds
--     & filter (\med -> day `elem` getDays med)
--     & map getName
--     & intersect names
--     & sort
--     & (==) (sort names)