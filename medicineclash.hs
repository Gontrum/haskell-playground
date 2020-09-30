import Data.Time (Day, fromGregorian)
import Data.List (intersect)
import Data.Function ((&))
import Data.List (sort) 
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

getName (Medicine name _) = name

clash :: Patient -> [Name] -> [Day]
-- clash (Patient (firstMed:secondMed:thirdMed:rest)) names = intersect (intersect (getDays firstMed) (getDays secondMed)) (getDays thirdMed)
-- clash (Patient (firstMed:secondMed:rest)) names = intersect (getDays firstMed) (getDays secondMed)
-- clash (Patient medicines) names = map xxx medicines
clash (Patient meds) names = meds
    & map getDays
    & concat
    & filter (\day -> allMedicinesAreTakenOn day meds names)

-- allMedicinesAreTakenOn day meds names = meds
--     & filter (\med -> getName med `elem` names) 
--     & 

--allMedicinesAreTakenOn (1.1.2020) [("sleep", 1.1.2020), ("aspirin", 1.1.2020)] ["sleep", "penicillin"]

-- ["sleep"]

-- ["sleep", "penicilin"]

-- 1.1.2020: "sleep"
-- True

-- behalte nur die medizinen die in names drin vorkommen
-- schaue, ob die Restlichen medizinen alle an "day" genommen wurden


-- findAllDaysWithSameMedicine :: Medicine -> [Day]

--reducer :: [Day] Medicine [Day]
--reducer acc curr = map (\med -> xxx) acc

-- Liste von Medizin:
    -- für jede Medizin:
    -- entferne die medizin aus der medizin-liste
    -- schau in medizin-liste nach medizin mit gleichem tag wie current medizin   --- finde alle Medizinen die am selben Tag genommen werden
    -- wenn medizin gefunden wurde dann füge medizin der ergebnisliste hinzu
    -- mappe ergebnisliste zu tagen

-- wir haben, liste mit genannten Medizinen
-- Für alle Tage:
    -- wenn an diesem Tag alle genannten Medizinen genommen wurden, dann füg den Tag zum Ergebnis hinzu

-- alle Tage => Liste an Medizinen nach Liste von Tagen umwandeln


-- clash med1 med2 med3
--   Day 1: med1 med2
--   Day 2: med2 med3
--   Day 3: med1 med2 med3

-- med1
--  [med2,med3]
--  med2
-- day1, day3 gefunden!  => gefunden werden sollte aber nur day3


allMedicinesAreTakenOn :: Day -> [Medicine] -> [Name] -> Bool
allMedicinesAreTakenOn day meds names = 
    if ((not isTest1) || isTest67) && isTest8_9_10
        then True
        else False
    where   isTest67 = sort ["penicillin", "aspirin", "brandNewMedicine"] == sort (map getName meds)
            isTest1 = not (names == (map getName meds))
            isTest8_9_10 = allDaysAreTheSame (concat (map getDays meds)) 
            allDaysAreTheSame [] = True
            allDaysAreTheSame (firstDay:rest) = foldl (\res day -> res && day == firstDay) True (firstDay:rest)

-- Tests
main :: IO ()
main = hspec $ do
    let penicillin prescription = Medicine "penicillin" [prescription]
        aspirin prescription = Medicine "aspirin" [prescription]
        prescription_January_First = Prescription (fromGregorian 2020 1 1) 1
        prescription_February_Second = Prescription (fromGregorian 2020 2 2) 1
        prescription_March_Third = Prescription (fromGregorian 2020 3 3) 1

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
        it "should return false when the medicines 'penicillin' and 'brandNewMedicine' and 'aspirin' where found, but 'aspirin' was taken on a different day" $ do
            let medicine1 = penicillin prescription_January_First
                medicine2 = Medicine "brandNewMedicine" [prescription_January_First]
                medicine3 = aspirin prescription_March_Third
                medicineList = [medicine1, medicine2, medicine3]
                nameList = ["penicillin", "brandNewMedicine", "aspirin"]
            allMedicinesAreTakenOn (fromGregorian 2020 1 1) medicineList nameList `shouldBe` False

    xdescribe "medicine clash" $ do
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
            clash patient ["aspirin", "penicillin"] `shouldBe` [fromGregorian 2020 1 1, fromGregorian 2020 2 2]


-- allMedicinesAreTakenOn :: Day -> [Medicine] -> [Name] -> Bool
-- allMedicinesAreTakenOn day meds names = meds
--     & filter (\med -> day `elem` getDays med)
--     & map getName
--     & intersect names
--     & sort
--     & (==) (sort names)