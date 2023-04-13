module Main (main) where

import Lib (createRecordList, formatYearsCosts, formatYearsLiters, totalCosts, totalLiters)
import Text.Printf (printf)

main :: IO ()
main = do
  content <- readFile "tanken.txt"
  let contentLines = lines content
  let records = createRecordList contentLines
  putStrLn "Jahreskosten:"
  putStrLn (mconcat (formatYearsCosts records))
  putStrLn "Jahresverbrauch:"
  putStrLn (mconcat (formatYearsLiters records))
  putStrLn (printf "Gesamtkosten: %7.2f Euro" (totalCosts records))
  putStrLn (printf "Gesamtliter:  %7.2f Liter" (totalLiters records))
