import Data.Function ((&))
import Data.Time (Day, fromGregorian, addDays)
import Test.Hspec ( hspec, describe, it, shouldBe )
import Data.List (groupBy,  intersect ) 

type Name = String
type DaysSupply = Integer
type DispenseDate = Day
data Prescription = Prescription DispenseDate DaysSupply
prescriptionForToday :: Prescription
prescriptionForToday = Prescription (fromGregorian 2020 9 29) 30

data Medicine = Medicine Name [Prescription]
getDays :: Medicine -> [DispenseDate]
getDays medicines = map (\(Prescription day _) -> day) (getPrescriptions medicines)
  where     getPrescriptions (Medicine name prescriptions) = prescriptions
addPrescription :: Medicine -> Prescription -> Medicine
addPrescription (Medicine name oldPrescriptions) prescription = Medicine name (prescription : oldPrescriptions)
getNames :: [Medicine] -> [Name]
getNames = map (\(Medicine name _) -> name)

data Patient = Patient [Medicine]
addMedicine :: Patient -> Medicine -> Patient
addMedicine (Patient oldMedicine) medicine = Patient $ medicine : oldMedicine

allMedicinesAreTakenOn :: DispenseDate -> [Medicine] -> [Name] -> Bool
allMedicinesAreTakenOn day meds names = names `subset` namesOfMedicinesTakenOn day
  where     namesOfMedicinesTakenOn day = getNames $ (medicinesTakenOn day) meds

medicinesTakenOn :: DispenseDate -> [Medicine] -> [Medicine]
medicinesTakenOn day = filter (\med -> day `isInDispenseTime` med)

isInDispenseTime :: DispenseDate -> Medicine -> Bool
isInDispenseTime day (Medicine _ prescriptions) = all condition prescriptions
    where   condition presciption = day <= lastDay presciption && day >= (firstDay presciption)
            firstDay (Prescription fd _) = fd
            lastDay (Prescription fd days) = addDays days fd

daysWhenMedicineWasTaken :: Medicine -> [DispenseDate]
daysWhenMedicineWasTaken (Medicine _ presciptions) = removeDuplicates $ concat $ map daysPerPrescription presciptions
daysPerPrescription :: Prescription -> [DispenseDate]
daysPerPrescription (Prescription _ 0) = []
daysPerPrescription (Prescription date numberOfDays) = date:(daysPerPrescription (Prescription (addDays 1 date) (numberOfDays - 1)))

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldl (\acc curr -> if curr `elem` acc then acc else curr : acc) []

subset :: (Foldable t, Eq a) => [a] -> t a -> Bool
subset a b = null [x | x <- a, (x `elem` b) == False]

clashx :: Patient -> [Name] -> [DispenseDate]
clashx (Patient meds) names = meds
    & map getDays
    & concat
    & filter (\day -> allMedicinesAreTakenOn day meds names)
    & removeDuplicates

clash :: Patient -> [Name] -> [DispenseDate]
clash (Patient meds) names = foldl reducer (daysWhenMedicineWasTaken (firstMedicine filteredMedicine)) filteredMedicine
    where reducer :: [DispenseDate] -> Medicine -> [DispenseDate]
          reducer acc med = intersect (daysWhenMedicineWasTaken med) acc
          filteredMedicine :: [Medicine]
          filteredMedicine = filter (\(Medicine name _) -> name `elem` names) meds
          firstMedicine :: [Medicine] -> Medicine
          firstMedicine (first:_) = first

mergeTwoOneMedicine :: [Medicine] -> Medicine
mergeTwoOneMedicine (firstMed:medicines) = foldl mergeTwoMedicines firstMed medicines
mergeTwoMedicines :: Medicine -> Medicine -> Medicine
mergeTwoMedicines (Medicine name prescriptions) (Medicine _ prescriptions2) = Medicine name $ prescriptions ++ prescriptions2

-- Tests
main :: IO ()
main = hspec $ do
  let penicillin prescription = Medicine "penicillin" [prescription]
      aspirin prescription = Medicine "aspirin" [prescription]
      prescription_January_First = Prescription (fromGregorian 2020 1 1) 1
      prescription_February_Second = Prescription (fromGregorian 2020 2 2) 1
      prescription_March_Third = Prescription (fromGregorian 2020 3 3) 1


  describe "daysWhenMedicineWasTaken" $ do
    it "Medicin with 32 days of supply should contain 32 days"  $ do
      length (daysWhenMedicineWasTaken (penicillin (Prescription (fromGregorian 2020 1 1) 32))) `shouldBe` 32
    it "Medicin with 0 days of supply should contain 0 days"  $ do
      length (daysWhenMedicineWasTaken (penicillin (Prescription (fromGregorian 2020 1 1) 0))) `shouldBe` 0
    it "Medicin with two prescriptions with 20 days each should contain 40 days"  $ do
      length (daysWhenMedicineWasTaken (Medicine "penicillin" [(Prescription (fromGregorian 2020 1 1) 20), (Prescription (fromGregorian 2020 12 1) 20)])) `shouldBe` 40
    it "Medicin with two prescriptions with 20 days each but with overlapping of 10 days should contain 30 days"  $ do
      length (daysWhenMedicineWasTaken (Medicine "penicillin" [(Prescription (fromGregorian 2020 1 1) 20), (Prescription (fromGregorian 2020 1 11) 20)])) `shouldBe` 30

  describe "isInDispenseTime" $ do 
      it "day inside dispense time should result in true" $ do
        isInDispenseTime (fromGregorian 2020 1 15) (Medicine "penicillin" [Prescription (fromGregorian 2020 1 1) 30]) `shouldBe` True
      it "first day of dispense time should result in true" $ do
        isInDispenseTime (fromGregorian 2020 1 1) (Medicine "penicillin" [Prescription (fromGregorian 2020 1 1) 30]) `shouldBe` True
      it "last day of dispense time should result in true" $ do
        isInDispenseTime (fromGregorian 2020 2 1) (Medicine "penicillin" [Prescription (fromGregorian 2020 1 1) 31]) `shouldBe` True
      it "day after last day of dispense time should result in false" $ do
        isInDispenseTime (fromGregorian 2020 2 1) (Medicine "penicillin" [Prescription (fromGregorian 2020 1 1) 30]) `shouldBe` False
      it "day before first day of dispense time should result in false" $ do
        isInDispenseTime (fromGregorian 2019 12 31) (Medicine "penicillin" [Prescription (fromGregorian 2020 1 1) 30]) `shouldBe` False
      it "day far away of dispense time should result in false" $ do
        isInDispenseTime (fromGregorian 1945 5 8) (Medicine "penicillin" [Prescription (fromGregorian 2020 1 1) 30]) `shouldBe` False

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
          patient = Patient [medicine1, medicine2]
      clash patient ["aspirin", "penicillin"] `shouldBe` []
    it "should return a clash when two medicines are taken on the same day" $ do
      let medicine1 = penicillin prescription_January_First
          medicine2 = aspirin prescription_January_First
          patient = Patient [medicine1, medicine2]
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
    it "should return a clash for two medicines within dispense time" $ do
      let medicine1_day1 = penicillin (Prescription (fromGregorian 2020 1 1) 90)
          medicine2_day2 = aspirin (Prescription (fromGregorian 2020 2 2) 1)
          patient = Patient [medicine1_day1, medicine2_day2]
      clash patient ["aspirin", "penicillin"] `shouldBe` [fromGregorian 2020 2 2]

-- allMedicinesAreTakenOn :: Day -> [Medicine] -> [Name] -> Bool
-- allMedicinesAreTakenOn day meds names = meds
--     & filter (\med -> day `elem` getDays med)
--     & map getName
--     & intersect names
--     & sort
--     & (==) (sort names)