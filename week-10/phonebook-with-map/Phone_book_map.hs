module Phone_book_map
  ( PhoneBook(..),
    Name,
    emptyBook,
    findEntries,
    addEntry
  ) where

{-
Phone book with Map

Make the module Phone_book_map export types PhoneBook, Name, and the following functions:
findEntries :: Name -> PhoneBook -> [Phone]
addEntry :: Name -> String -> String -> String -> [Integer] -> PhoneBook -> PhoneBook
emptyBook :: PhoneBook

examples:
addEntry "PersonA" "WorkLandline" "358" "123456789" [358] emptyBook
-}

import Phone_type2

import qualified Data.Map as Map
type Name = String
-- key= Name, value = list of phones 
type PhoneBook = Map.Map Name [Phone]

emptyBook :: PhoneBook
emptyBook = Map.empty

addEntry :: Name -> String -> String -> String -> [Integer] -> PhoneBook -> PhoneBook
addEntry "" _ _ _ _ currentbook = currentbook 
addEntry name phonetype ccode phonenum ccodelist book
  | Map.null book = Map.insert name [newPhone] emptyBook
  | foundPhones == [] = Map.insert name [newPhone] book
  | sameNameAndnumber == [] = Map.insertWith (++) name [newPhone] book -- add new phone to existing name
  | otherwise = book
  where newPhone = (readPhone phonetype ccode phonenum ccodelist)
        foundPhones = findEntries name book -- returns list of Phones
        sameNameAndnumber = filter (\item -> show (phoneNo item) == ( "PhoneNo " ++ phonenum)) foundPhones

-- Find a list of Phones by a name
findEntries :: Name -> PhoneBook -> [Phone]
findEntries "" _ = []
findEntries nameToFind phonebook
  | Map.null phonebook = []  -- if map is empty, return empty list
findEntries nameToFind phonebook
  | Map.null checkMap = []  -- if map is empty, return empty list
  | otherwise =  phonesInList (Map.lookup nameToFind checkMap)
  where checkMap = Map.filterWithKey (\k _ -> k == nameToFind) phonebook

-- filterWithKey filters all keys/values that satisfy the predicate:
-- filterWithKey (\k _ -> k > 4) (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"


-- because Map.lookup returns Maybe a, this returns only the a part or empty list
phonesInList :: Maybe [Phone] -> [Phone]
phonesInList Nothing = []
phonesInList (Just a) = a

-- Find a list of entries by a name, returns phonebook
-- findEntries :: String -> PhoneBook -> PhoneBook
-- findEntries "" _ = emptyBook
-- findEntries nameToFind phonebook
--   | phonebook == Map.empty = emptyBook
--   | otherwise = Map.filterWithKey (\k _ -> k == nameToFind) phonebook
