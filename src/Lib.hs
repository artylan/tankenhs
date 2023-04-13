{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( Date (Date, year, month, day),
    Record (Record, date, km, liters, costs),
    parseDate,
    parseRecord,
    createRecordList,
    totalCosts,
    totalLiters,
    formatYearsCosts,
    formatYearsLiters,
  )
where

import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Text (pack, replace, unpack)
import Text.Printf (printf)

data Date = Date
  { year :: Int,
    month :: Int,
    day :: Int
  }
  deriving (Eq, Show)

parseDate :: String -> Date
parseDate text = Date myYear myMonth myDay
  where
    parts = splitOn "." text
    myDay = read (head parts)
    myMonth = read (parts !! 1)
    myYear = read (parts !! 2)

data Record = Record
  { date :: Date,
    km :: Int,
    liters :: Double,
    costs :: Double
  }
  deriving (Eq, Show)

parseRecord :: String -> Record
parseRecord text = Record myDate myKm myLiters myCosts
  where
    parts = splitOn "\t" text
    myDate = parseDate (head parts)
    myKm = read (parts !! 1)
    myLiters = parseGermanDouble (parts !! 2)
    myCosts = parseGermanDouble (parts !! 3)

replaceGermanComma :: String -> String
replaceGermanComma = unpack . replace "," "." . pack

parseGermanDouble :: String -> Double
parseGermanDouble text = read (replaceGermanComma text)

createRecordList :: [String] -> [Record]
createRecordList = map parseRecord

totals :: (Record -> Double) -> [Record] -> Double
totals selector = foldl (\x y -> x + selector y) 0.0

totalCosts :: [Record] -> Double
totalCosts = totals (\x -> x.costs)

totalLiters :: [Record] -> Double
totalLiters = totals (\x -> x.liters)

extractYears :: [Record] -> [Int]
extractYears records = nub (map (\x -> x.date.year) records)

sumYear :: (Record -> Double) -> [Record] -> Int -> (Int, Double)
sumYear selector records aYear = (aYear, foldl (\x y -> x + selector y) 0.0 (filter (\x -> x.date.year == aYear) records))

sumYears :: (Record -> Double) -> [Record] -> [(Int, Double)]
sumYears selector records = map (sumYear selector records) years
  where
    years = extractYears records

formatYears :: (Record -> Double) -> String -> [Record] -> [String]
formatYears selector aUnit records = map (\(x, y) -> printf "%d : %7.2f %s\n" x y aUnit) (sumYears selector records)

formatYearsCosts :: [Record] -> [String]
formatYearsCosts = formatYears (\r -> r.costs) "Euro"

formatYearsLiters :: [Record] -> [String]
formatYearsLiters = formatYears (\r -> r.liters) "Liter"