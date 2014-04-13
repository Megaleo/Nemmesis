module Timeline where

-- | The months in portuguese in order
data Month = Janeiro -- ^ January
           | Fevereiro -- ^ February
           | Março -- ^ March
           | Abril -- ^ April
           | Maio -- ^ May
           | Junho -- ^ June
           | Julho -- ^ July
           | Agosto -- ^ August
           | Setembro -- ^ Septembaer
           | Outubro -- ^ October
           | Novembro -- ^ November
           | Dezembro -- ^ December
           deriving (Eq, Show, Enum)

-- | @intToMonth n@ gives the month that ocuppies the position @n@
-- in the calendar, starting with the January as 1.
intToMonth :: Int -> Month
intToMonth m = toEnum $ m - 1

-- | @monthToInt n@ is the opposite of the intToMonth: it gives the
-- the position of the month, starting with the January as 1.
monthToInt :: Month -> Int
monthToInt m = 1 + fromEnum m

-- | The date.
data Date = Date { day :: Int
                 , month :: Month
                 , year :: Int
                 } deriving (Eq)

-- Shows the date using the bar '/'
instance Show Date where
    show (Date d m y) = showD ++ "/" ++ showM ++ "/" ++ (show y)
        where
            showM = if length (show $ monthToInt m) == 1
                then "0" ++ (show $ monthToInt m)
                else show $ monthToInt m
            showD = if length (show d) == 1
                then "0" ++ (show d)
                else show d

-- | Shows how the date would be if written (in Portuguese)
writtenDate :: Date -> String
writtenDate (Date d m y) = (show d) ++ " de " ++ (show m) ++ " de " ++ (show y)

-- | An event in history. It contains the date that date of occourence
-- and some description
-- Maybe an image later
data Event = Event { date :: Date
                   , description :: String
                   } deriving (Eq, Show)

-- | Contains all the information and the events about some theme
-- the user wants to give focus.
data Timeline = Timeline { name :: String -- ^ Name of the timeline
                         , range :: (Int, Int) -- ^ Where the timeline starts and begins
                         , events :: [Event] -- ^ The events in the timeline
                         } deriving (Eq, Show)
