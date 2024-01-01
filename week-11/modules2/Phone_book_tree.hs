module Phone_book_tree
  ( PhoneBook(Empty, Node),
    Name,
    emptyBook,
    findEntries,
    addEntry
  ) where

{-
Binary search tree (BST)
an element points to two elements, one on its left and one on its right.
The element to the left is smaller, the element to the right is bigger.
-}

import Phone_type2
type Name = String
data PhoneBook = Empty | Node String [Phone] PhoneBook PhoneBook deriving (Show,Eq)

emptyBook :: PhoneBook
emptyBook = Empty

addEntry :: Name -> String -> String -> String -> [Integer] -> PhoneBook -> PhoneBook
addEntry "" _ _ _ _ currentbook = currentbook 
addEntry name phonetype ccode phonenum ccodelist Empty = Node name [(readPhone phonetype ccode phonenum ccodelist)] Empty Empty
addEntry name phonetype ccode phonenum ccodelist (Node f list left right) 
  | name < f = Node f list (addEntry name phonetype ccode phonenum ccodelist left ) right
  | name > f = Node f list left (addEntry name phonetype ccode phonenum ccodelist right )
  | otherwise = case foundPhones of
                  [] -> Node name [newPhone] left right
                  _ -> if ( sameNameAndnumber == [] ) 
                    then Node name (newPhone : foundPhones) left right -- add new phone to existing name
                    else Node name foundPhones left right  
  where newPhone = (readPhone phonetype ccode phonenum ccodelist)
        book = (Node f list left right) 
        foundPhones = findEntries name book
        sameNameAndnumber = filter (\item -> show (phoneNo item) == ( "PhoneNo " ++ phonenum)) foundPhones

-- Find a list of Phones by a name
findEntries :: Name -> PhoneBook -> [Phone]
findEntries "" _ = []
findEntries _ Empty = []
findEntries nameToFind (Node nameInBook list left right)
  | nameToFind == nameInBook = list
  | nameToFind < nameInBook = findEntries nameToFind left
  | nameToFind > nameInBook = findEntries nameToFind right
  