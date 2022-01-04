module Chapter12 where

-- Chapter 12
-- Rewrite patientInfo to use your patientName type,
-- reducing the total arguments needed to three instead of four

type PatientName = (String, String)

type FirstName = String
type LastName = String
type MiddleName = String

data Name = Name FirstName LastName | NameWithMiddle FirstName MiddleName LastName

patientInfo :: PatientName -> Int -> Int -> String
patientInfo name age height = fullname ++ " " ++ ageHeight
  where
    fullname = fst name ++ ", " ++ snd name
    ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

data Sex = Male | Female | Woke | Undecided | Hermaphrodite

data RhType = Pos | Neg

data ABOType = A | B | AB | O

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'
sexInitial Woke = 'W'

data BloodType = BloodType ABOType RhType

data Patient = Patient
  { name :: Name,
    sex :: Sex,
    age :: Int,
    height :: Int,
    weight :: Int,
    bloodType :: BloodType
  }
  
naomiRosenberg :: Patient
naomiRosenberg = Patient{ name = Name "Naomi" "Rosenberg", age = 43, sex = Female, height = 64, weight = 147, bloodType = BloodType AB Neg }
mikeHarris = Patient{ name = Name "Mike" "Harris", age = 51, sex = Woke, height = 100, weight = 1000, bloodType = BloodType O Neg }
showName:: Name -> String
showName (Name first last) = first ++ " " ++ last
showName (NameWithMiddle first middle last) = first ++ " "  ++ middle ++  " " ++ last

origCanDonateTo (BloodType O _) _ = True
origCanDonateTo _ _ = False

canDonateTo :: Patient -> Patient -> Bool
canDonateTo patientA patientB = origCanDonateTo (bloodType patientA) (bloodType patientB)

patientSummary aPatient = "****************\nPatient Name: " ++ showName (name aPatient) 