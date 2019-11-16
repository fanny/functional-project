{-# LANGUAGE DeriveGeneric #-}

module GregorianCalendar (
  GregorianCalendar(..)
) where

import Data.Aeson
import GHC.Generics

data GregorianCalendar = GregorianCalendar {
  year :: Integer,
  month :: Integer,
  dayOfMonth :: Integer
} deriving (Show, Generic)

instance FromJSON GregorianCalendar
instance ToJSON GregorianCalendar