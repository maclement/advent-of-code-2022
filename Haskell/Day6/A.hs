import Data.List (nub)

main :: IO ()
main = input (solve 4) >> input (solve 14)

input :: Show a => (String -> a) -> IO ()
input f  = readFile ".\\input.txt"  >>= print . f

solve :: Int -> String -> Int
solve marker s = marker + minimum [ i | i <- [0..length s - marker], let inf = take marker $ drop i s, nub inf == inf]
  