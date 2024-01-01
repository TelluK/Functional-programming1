-- Interactive phone book

{-
commands that program understands:
add name phone_type country_code phone_no
find name
quit

-- 
add Bob Other +358 123545
add Bob WorkLandline +358 123456789
find Bob
-}

import PhoneBook

initialState = emptyBook

getPhone :: PhoneBookEntry -> Phone
getPhone = phone

eventLoop :: PhoneBook -> IO String
eventLoop state =
  do
    line <- getLine
    putStrLn ("> " ++ line)
    case line of
      "" -> eventLoop state
      "quit" -> return "bye"
      _ -> do
        let wordsList = words line
        let len = length wordsList
        let firstStr = head wordsList
        case firstStr of
          "add" -> do
            let countrycodes = [358, 44] 
            -- putStrLn ("wordsList length : " ++ (show len))
            if (len == 5)
              then do
                let updatedPhoneBook = addEntry (wordsList !! 1) (wordsList !! 2) (wordsList !! 3) (wordsList !! 4) countrycodes state
                putStrLn "Done"
                eventLoop updatedPhoneBook
              else do
                putStrLn "Cannot do that"
                eventLoop state
          "find" -> do
            if (len == 2)
              then do
                let foundPhoneBook = findEntries (wordsList !! 1) state
                let phones = map getPhone foundPhoneBook
                putStrLn $ show phones
                eventLoop state
              else do
                putStrLn "Cannot do that"
                eventLoop state
          _ -> do
            putStrLn "Cannot do that"
            eventLoop state


-- program can be compiled (with main function)
main :: IO ()
main = do
  putStrLn "Welcome to phone book application"
  final <- eventLoop initialState
  putStrLn final
