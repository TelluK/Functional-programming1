-- Simple Calculator
{-
For example: 
calculate ["3 + 5","a + 3", "4 - 3"]
-> ["8", "I cannot calculate that", "1"]
-}

calculate :: [String] -> [String]
calculate list = map calc list


calc :: String -> String
calc str =
  case words str of    -- words "3 + 5" -> ["3", "+", "5"]
    [a, operation, b] ->
      case (readMaybe a, readMaybe b) of 
        (Just x, Just y) ->
          case operation of 
            "+" -> show (x + y)
            "-" -> show (x - y)
            "*" -> show (x * y)
            _   -> "I cannot calculate that"
        _ -> "I cannot calculate that"
    _ -> "I cannot calculate that"


-- try to interpret string as value of another type
-- example: (readMaybe "123") :: Maybe Integer returns Just 123,
-- (readMaybe "123thisfails") :: Maybe Integer returns Nothing
readMaybe :: (Read a) => String -> Maybe a
readMaybe str = case reads str of
                    [(x,"")] -> Just x
                    _ -> Nothing


-- reads "123" -> [(123,"")]
-- reads "xyxy" -> [] 
-- reads "123xyxy" -> [(123,"xyxy")]