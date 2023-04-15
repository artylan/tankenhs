module Main (main) where

import Lib (createRecordList, formatYearsCosts, formatYearsLiters, totalCosts, totalKilometers, totalLiters, consumption)
import Text.Printf (printf)

main :: IO ()
main = do
  content <- readFile "tanken.txt"
  let contentLines = lines content
  let records = createRecordList contentLines

  putStrLn "Kosten pro Jahr:"
  putStrLn "----------------"
  putStrLn (mconcat (formatYearsCosts records))

  putStrLn "Liter pro Jahr:"
  putStrLn "---------------"
  putStrLn (mconcat (formatYearsLiters records))

  putStrLn (printf "Gefahrene Kilometer: %d." (totalKilometers records))
  putStrLn ""

  putStrLn (printf "Gesamtverbrauch: %7.2f Liter." (totalLiters records))
  putStrLn ""

  putStrLn (printf "Gesamtkosten: %7.2f Euro." (totalCosts records))
  putStrLn ""

  putStrLn (printf "Durchnittliche Kosten: %.2f Euro pro Liter." (totalCosts records / totalLiters records))
  putStrLn ""

  putStrLn (printf "Verbrauch: %.2f Liter pro 100 km." (consumption records))
  putStrLn ""

  putStrLn (printf "Kosten: %.2f Euro pro 100 km." (consumption records * totalCosts records / totalLiters records))
  putStrLn ""
