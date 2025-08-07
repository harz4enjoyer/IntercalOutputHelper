import Data.Char (ord)

main :: IO ()
main = do
  input <- getLine
  let intercalIndex = convertToIntercal input
  let intercalCode = generateIntercal intercalIndex
  putStrLn (show intercalCode)
  writeFile "output.intercal" intercalCode

convertToIntercal :: String -> [Int]
convertToIntercal input = add256 $ sequentialSubtract $ toDecimal $ toReverse $ toBinary $ toASCII input

toASCII :: String -> [Int]
toASCII = map ord

toBinary :: [Int] -> [String]
toBinary = map decToBin

toReverse :: [String] -> [String]
toReverse = map reverse

toDecimal :: [String] -> [Int]
toDecimal = map binToDec

add256 :: [Int] -> [Int]
add256 = map (+ 256)

decToBin :: Int -> String
decToBin n
  | n < 0 = error "Input must be non-negative"
  | n > 255 = error "Only 8-bit values supported"
  | otherwise =
      let bin = binDigits n
       in replicate (8 - length bin) '0' ++ bin

binDigits :: Int -> String
binDigits 0 = "0"
binDigits n =
  let (q, r) = n `divMod` 2
   in binDigits q ++ show r

binToDec :: String -> Int
binToDec binary = foldl (\acc digit -> acc * 2 + digitToInt digit) 0 binary
  where
    digitToInt '0' = 0
    digitToInt '1' = 1
    digitToInt _ = error "Invalid binary digit"

sequentialSubtract :: [Int] -> [Int]
sequentialSubtract [] = []
sequentialSubtract [x] = [0 - x]
sequentialSubtract (x : xs) = (0 - x) : zipWith (-) (x : xs) xs

generateIntercal :: [Int] -> String
generateIntercal values =
  let header = "DO ,1 <- #" ++ show (length values)
      assignments = zipWith formatLine [1 ..] values
      footer = ["PLEASE READ OUT ,1", "PLEASE GIVE UP"]
   in unlines $ [header] ++ assignments ++ footer
  where
    formatLine :: Int -> Int -> String
    formatLine index value = "DO ,1 SUB #" ++ show index ++ " <- #" ++ show value
