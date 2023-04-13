import Lib (Date (Date), Record(Record), parseDate, parseRecord)

assert :: Bool -> String -> String -> IO ()
assert test passStatement failStatement =
  if test
    then putStrLn passStatement
    else putStrLn failStatement

main :: IO ()
main = do
  putStrLn "Running tests..."
  assert (parseDate "7.02.1968" == Date 1968 2 7) "passed 'parseDate'" "FAIL: 'parseDate'"
  let d = Date 2016 2 28
  let r = Record d 12942 33.15 38.75
  assert (parseRecord "28.02.2016\t12942\t33,15\t38,75" == r) "passed 'parseRecord'" "FAIL: 'parseRecord'"
