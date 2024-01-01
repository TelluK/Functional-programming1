module Calendar 
  ( emptyCalendar,
    CalendarEntry(..),
    Calendar(..),
    addEntry,
    findByName,
    findByPlace,
    findByDate,
  ) where


emptyCalendar = []

data CalendarEntry = CalendarEntry { name :: String, place :: String, date :: String } deriving ( Eq, Show)
type Calendar = [ CalendarEntry]

addEntry :: String -> String -> String -> Calendar -> Calendar
addEntry name place date [] = CalendarEntry { name = name , place = place, date = date} : emptyCalendar  
addEntry name place date currentCalendar = 
  if filteredCalendar == []
    then CalendarEntry { name = name , place = place, date = date} : currentCalendar
    else CalendarEntry { name = name , place = place, date = date} : (removeOldEvent name currentCalendar)
  where
    filteredCalendar = findByName name currentCalendar

removeOldEvent :: String -> Calendar -> Calendar
removeOldEvent "" [] = emptyCalendar
removeOldEvent _ [] = emptyCalendar
removeOldEvent nameToFind calendar = filter (\e -> name e /= nameToFind) calendar

-- Find a list of entries by a name
findByName :: String -> Calendar -> Calendar
findByName "" _ = emptyCalendar
findByName _ [] = emptyCalendar
findByName nameToFind calendar = filter (\x -> name x == nameToFind) calendar

-- Find a list of entries by a place
findByPlace :: String -> Calendar -> Calendar
findByPlace "" _ = emptyCalendar
findByPlace _ [] = emptyCalendar
findByPlace placeToFind calendar = filter (\x -> place x == placeToFind) calendar

-- Find a list of entries by a date
findByDate :: String -> Calendar -> Calendar
findByDate "" _ = emptyCalendar
findByDate _ [] = emptyCalendar
findByDate dateToFind calendar = filter (\x -> date x == dateToFind) calendar