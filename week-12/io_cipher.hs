-- Simple IO cipher

import qualified Data.Map

encode :: Int -> String -> String
encode shift msg = map (charmap Data.Map.!) msg
  where charlist = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']
        listlength = length charlist
        shiftedlist = take listlength (drop (shift `mod` listlength) (cycle charlist))
        charmap = Data.Map.fromList $ zip charlist shiftedlist

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x,"")] -> Just x
                                _ -> Nothing


eventLoop ::  IO String
eventLoop = 
  do
    inputLine <- getLine
    putStrLn ("> " ++ inputLine)
    case inputLine of
      "" -> eventLoop
      "quit" -> return "bye" -- program will end
      _ -> do
        let wordsList = words inputLine
        let len = length wordsList
        -- putStrLn ("List lenght: " ++ (show len))
        let firstStr = head wordsList
        if (len <= 2)
          then do
            putStrLn "I cannot do that"
            eventLoop -- recursive call
          else do
            let numStr = head (tail wordsList)
            case (readMaybe numStr) of -- trying to read to int
              (Just num ) -> do
                let otherWords = drop 2 wordsList
                -- putStrLn (show otherWords)
                case firstStr of
                  "encode" -> do
                    let encodedList = map (encode num) otherWords
                    putStrLn $ unwords encodedList
                    eventLoop -- recursive call
                  "decode" -> do
                    let decodedList = map (decode num)  otherWords
                    putStrLn $ unwords decodedList
                    eventLoop -- recursive call
                  _ ->  do
                    putStrLn "I cannot do that"
                    eventLoop -- recursive call
              _ -> do
                putStrLn "I cannot do that"
                eventLoop -- recursive call

-- program can be compiled (with main function)
main = do
  final <- eventLoop
  putStrLn final

