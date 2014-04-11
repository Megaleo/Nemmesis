module Timeline where

data Date = Date { day :: Int
                 , month :: Int
                 , year :: Int
                 } deriving (Eq)

data Event = Event { date :: Date
                   , description :: String
                   } deriving (Eq, Show)

data Timeline = Timeline { name :: String
                         , range :: (Int, Int)
                         , events :: [Event]
                         } deriving (Eq, Show)

