module Timeline where

data Month = Janeiro
           | Fevereiro
           | Março
           | Abril
           | Maio
           | Junho
           | Julho
           | Agosto
           | Setembro
           | Outubro
           | Novembro
           | Dezembro
           deriving (Eq, Show, Enum)

intToMonth :: Int -> Month
intToMonth m = toEnum $ m - 1

monthToInt :: Month -> Int
monthToInt m = 1 + fromEnum m

data Date = Date { day :: Int
                 , month :: Month
                 , year :: Int
                 } deriving (Eq)

instance Show Date where
    show (Date d m y) = showD ++ "/" ++ showM ++ "/" ++ (show y)
        where
            showM = if length (show $ monthToInt m) == 1
                then "0" ++ (show $ monthToInt m)
                else show $ monthToInt m
            showD = if length (show d) == 1
                then "0" ++ (show d)
                else show d


writtenDate :: Date -> String
writtenDate (Date d m y) = (show d) ++ " de " ++ (show m) ++ " de " ++ (show y)

data Event = Event { date :: Date
                   , description :: String
                   } deriving (Eq, Show)

data Timeline = Timeline { name :: String
                         , range :: (Int, Int)
                         , events :: [Event]
                         } deriving (Eq, Show)

