import Data.Char (isLower, ord)
import Data.List (intersect, nub)
import Data.Bool (bool)

main :: IO ()
main = input solve1 >> input solve2

input :: Show a => ([String] -> a) -> IO ()
input f  = readFile ".\\input.txt"  >>= print . f .  lines

value :: Char -> Int
value c = bool (ord c - 38) (ord c - 96) (isLower c)

solve1 :: [String] -> Int
solve1 = sum . map (\s -> value . head . nub . uncurry intersect . splitAt (length s `div` 2) $ s)


solve2 :: [String] -> Int
solve2 = sum . map (value . head . nub . foldr1 intersect) . chunks
 where
  chunks :: [a] -> [[a]]
  chunks (x:y:z:l) = [x,y,z] : chunks l
  chunks _         = []