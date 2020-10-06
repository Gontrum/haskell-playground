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
mergeMedicineWithGivenNames :: [Medicine] -> [Name] -> [Medicine]
mergeMedicineWithGivenNames medicines names = map (\name -> (mergeToSingleMedicine (filterByName name medicines))) names
  where   filterByName :: Name -> [Medicine] -> [Medicine]
          filterByName name medicines = filter (\(Medicine medName _) -> name == medName) medicines
mergeToSingleMedicine :: [Medicine] -> Medicine
mergeToSingleMedicine (firstMed:medicines) = foldl mergeTwoMedicines firstMed medicines
mergeTwoMedicines :: Medicine -> Medicine -> Medicine
mergeTwoMedicines (Medicine name prescriptions) (Medicine _ prescriptions2) = Medicine name $ prescriptions ++ prescriptions2

data Patient = Patient [Medicine]
addMedicine :: Patient -> Medicine -> Patient
addMedicine (Patient oldMedicine) medicine = Patient $ medicine : oldMedicine

daysWhenMedicineWasTaken :: Medicine -> [DispenseDate]
daysWhenMedicineWasTaken (Medicine _ presciptions) = removeDuplicates $ concat $ map daysPerPrescription presciptions
daysPerPrescription :: Prescription -> [DispenseDate]
daysPerPrescription (Prescription _ 0) = []
daysPerPrescription (Prescription date numberOfDays) = date:(daysPerPrescription (Prescription (addDays 1 date) (numberOfDays - 1)))

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldl (\acc curr -> if curr `elem` acc then acc else curr : acc) []

clash :: Patient -> [Name] -> [DispenseDate]
clash (Patient meds) names = foldl intersectAll daysOfFirstMedicine filteredMedicine
    where intersectAll :: [DispenseDate] -> Medicine -> [DispenseDate]
          intersectAll acc med = intersect acc $ daysWhenMedicineWasTaken med
          filteredMedicine = mergeMedicineWithGivenNames meds names
          daysOfFirstMedicine = daysWhenMedicineWasTaken $ firstMedicine filteredMedicine
          firstMedicine :: [Medicine] -> Medicine
          firstMedicine (first:_) = first

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
