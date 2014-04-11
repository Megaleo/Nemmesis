module Timeline where

data Date = Date { day :: Int
                 , month :: Int
                 , year :: Int
                 } deriving (Eq)

instance Show Date where
    show (Date d m y)
        | length (show m) == 1 = (show d) ++ "/0" ++ (show m) ++ "/" ++ (show y)
        | otherwise = (show d) ++ "/" ++ (show m) ++ "/" ++ (show y)

data Event = Event { date :: Date
                   , description :: String
                   } deriving (Eq, Show)

data Timeline = Timeline { name :: String
                         , range :: (Int, Int)
                         , events :: [Event]
                         } deriving (Eq, Show)

