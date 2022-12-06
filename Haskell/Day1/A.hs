import Data.List (sort, groupBy)
import Data.Char (isDigit)
import Data.Function (on)

example :: [[Int]] 
example = [[1000, 2000, 3000], [4000], [5000, 6000], [7000, 8000, 9000], [10000]]

solve1 :: [[Int]] -> Int
solve1 = maximum . map sum

solve2 :: [[Int]] -> Int
solve2 = sum . take 3 . reverse . sort . map sum

input :: Show a => ([[Int]] -> a) -> IO ()
input f  = readFile ".\\input.txt"  
         >>= print
         . f
         . map (map read)                                            -- parse
         . map (filter (not . null))                                 -- remove empty strings from groups
         . groupBy ((==) `on` (\x -> all isDigit x && length x > 0)) -- empty lines seperate groups
         . lines                                                     -- unline the file

main :: IO ()
main = input solve1 >> input solve2


