{-
commands that program understands:
Event <name> happens at <place> on <date>
Tell me about <eventname>
What happens on <date>
What happens at <place>
Quit

The values are given inside of single quotation marks. The date format is YYYY-MM-DD.
Event 'Event X' happens at 'Place Y' on '2019-10-08'
Tell me about 'Event X'
What happens on '2019-10-08'
What happens at 'Place Y'
-}

import Calendar
import Dates

import Data.List (groupBy, sortBy)
import Data.Function (on)

isQuotationMark :: Char -> Bool
isQuotationMark c = c == '\''

isLineMark :: Char -> Bool
isLineMark c = c == '-'

partition:: String -> [String]
partition = groupBy ((==) `on` isQuotationMark)


eventLoop state = do
  line <- getLine
  putStrLn ("> " ++ line)
  case line of
    "Quit" -> return "bye"
    _ -> do
      let wordsList = words line
      let parts = filter (not . null) $ partition line 
      let lenP = length parts
      case parts of 
        -- Tell me about 'Event X'
        [ "Tell me about ", "'",  eventname, "'"] -> do
          let foundEvent = findByName eventname state
          case foundEvent of
            [] -> do 
              putStrLn "I do not know of such event"
            _ -> do
              printEventInfo foundEvent
          eventLoop state
        -- What happens on '2019-10-08'
        [ "What happens on ", "'",  date, "'"] -> do
          let dateParts = filter (not . null) $ (groupBy ((==) `on` isLineMark)) date
          case dateParts of
            [year, "-", month, "-", day] -> do
              let isGivenDateCorrect = correctDate ( read year :: Integer) ( read month :: Integer) ( read day :: Integer)
              if isGivenDateCorrect
                then do 
                  let foundEvent = findByDate date state
                  case foundEvent of
                    [] -> do 
                      putStrLn "Nothing that I know of"
                    _ -> do 
                      printEventWithDate foundEvent
                else do 
                  putStrLn "Bad date"
            _ -> do
              putStrLn "Bad date"
          eventLoop state
        -- What happens at 'Place Y'
        [ "What happens at ", "'",  place, "'"] -> do
          let foundEvent = findByPlace place state
          case foundEvent of
            [] -> do 
              putStrLn "Nothing that I know of"
            _ -> do 
              printEventWithPlace foundEvent
          eventLoop state
          -- Event 'Event X' happens at 'Place Y' on '2019-10-08'
        [ "Event ", "'", name, "'", " happens at ", "'", place, "'", " on ","'", date, "'" ] -> do
          let dateParts = filter (not . null) $ (groupBy ((==) `on` isLineMark)) date
          case dateParts of
            [year, "-", month, "-", day] -> do
              let isGivenDateCorrect = correctDate ( read year :: Integer) ( read month :: Integer) ( read day :: Integer)
              if isGivenDateCorrect
                then do  
                  putStrLn "Ok"
                  let newCalendar = addEntry name place date state
                  eventLoop newCalendar
                else do 
                  putStrLn "Bad date"
                  eventLoop state
            _ -> do
              putStrLn "Bad date"
              eventLoop state
        _ -> do
          whenWrongCommand
          eventLoop state


printEventInfo foundEvents = do
   mapM_ printOne foundEvents
   where 
    printOne (CalendarEntry name place date) =
      putStrLn $ ("Event " ++ name  ++ " happens at " ++ place ++ " on " ++ date)

printEventWithPlace foundEvents = do
   mapM_ printOne $ sortBy (\x y -> compare (name x) (name y)) foundEvents
   where 
    printOne (CalendarEntry name place date) =
      putStrLn $ ("Event " ++ name  ++ " happens at " ++ place)


printEventWithDate foundEvents = do
   mapM_ printOne $ sortBy (\x y -> compare (name x) (name y)) foundEvents
   where 
    printOne (CalendarEntry name place date) =
      putStrLn $ ("Event " ++ name ++ " happens on " ++ date)
      

whenWrongCommand = do
  putStrLn "I do not understand that. I understand the following:"
  putStrLn "*Event <name> happens at <place> on <date>"
  putStrLn "*Tell me about <eventname>"
  putStrLn "*What happens on <date>"
  putStrLn "*What happens at <place>"
  putStrLn "*Quit"


initialState = emptyCalendar

-- program can be compiled (with main function)
main :: IO ()
main = do
  final <- eventLoop initialState
  putStrLn final