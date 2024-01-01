module Phone_book
  ( PhoneBook(..),
    emptyBook,
    findEntries,
    addEntry
  ) where

import Phone_type2

emptyBook :: PhoneBook
emptyBook = []

data PhoneBookEntry = PhoneBookEntry { name :: String , phone :: Phone } deriving (Eq, Show)
type PhoneBook = [PhoneBookEntry]  -- type synonym

addEntry :: String -> String -> String -> String -> [Integer] -> PhoneBook -> PhoneBook
addEntry "" _ _ _ _ currentbook = currentbook 
addEntry name phonetype ccode phonenum ccodelist [] = 
  PhoneBookEntry { name=name , phone= (readPhone phonetype ccode phonenum ccodelist) } : emptyBook
addEntry name phonetype ccode phonenum ccodelist currentbook = 
  if filteredBook == []
    then PhoneBookEntry { name=name , phone= newPhone } : currentbook
    else if sameNameAndnumber == []
      then PhoneBookEntry { name=name , phone= newPhone } : currentbook
      else currentbook
      where filteredBook = findEntries name currentbook
            newPhone = (readPhone phonetype ccode phonenum ccodelist)
            sameNameAndnumber = filter (\item -> show (phoneNo (phone item)) == ( "PhoneNo " ++ phonenum) ) filteredBook

-- Find a list of entries by a name
findEntries :: String -> PhoneBook -> PhoneBook
findEntries "" _ = emptyBook
findEntries _ [] = emptyBook
findEntries nameToFind phonebook = filter (\x -> name x == nameToFind) phonebook
