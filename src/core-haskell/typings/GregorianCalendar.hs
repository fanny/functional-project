module GregorianCalendar (
  GregorianCalendar(..)
) where

data GregorianCalendar = GregorianCalendar {
  year :: Integer,
  month :: Integer,
  dayOfMonth :: Integer
} deriving (Show);