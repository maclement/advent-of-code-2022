import Text.Parsec.Char
import Text.Parsec (Parsec, digit, many, parseTest, many1, many, (<|>), anyChar, sepBy, parse)
import Data.Bifunctor (bimap)
import Text.Parsec.Combinator (option)
import Data.Functor(($>))
import Data.List (sort)

main1 :: Int -> IO ()
main1 i = readFile "./input.txt"  >>= \s -> either print (print . solve1 i) (parse parser "" s)

main2 :: IO ()
main2 = readFile "./input.txt" >>= \s -> either print (print . solve2 (0, 4000000)) (parse parser "" s)

exampleInput :: [String]
exampleInput = [ "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
               , "Sensor at x=9, y=16: closest beacon is at x=10, y=16"
               , "Sensor at x=13, y=2: closest beacon is at x=15, y=3"
               , "Sensor at x=12, y=14: closest beacon is at x=10, y=16"
               , "Sensor at x=10, y=20: closest beacon is at x=10, y=16"
               , "Sensor at x=14, y=17: closest beacon is at x=10, y=16"
               , "Sensor at x=8, y=7: closest beacon is at x=2, y=10"
               , "Sensor at x=2, y=0: closest beacon is at x=2, y=10"
               , "Sensor at x=0, y=11: closest beacon is at x=2, y=10"
               , "Sensor at x=20, y=14: closest beacon is at x=25, y=17"
               , "Sensor at x=17, y=20: closest beacon is at x=21, y=22"
               , "Sensor at x=16, y=7: closest beacon is at x=15, y=3"
               , "Sensor at x=14, y=3: closest beacon is at x=15, y=3"
               , "Sensor at x=20, y=1: closest beacon is at x=15, y=3"
               ]

type Point = (Int, Int)
type Parser = Parsec String ()

num :: Parser Int
num = option id (char '-' $> (* (-1))) <*> (read <$> many1 digit)

point :: Parser (Int, Int)
point = (,) <$> (string "x=" *> num <* char ',' <* spaces) <*> (string "y=" *> num)

sensor :: Parser Sensor
sensor = (\p1 p2 -> Sensor p1 p2 (dist p1 p2)) <$> (string "Sensor at " *> point) <*> (string ": closest beacon is at " *> point)

parser :: Parser [Sensor]
parser = sensor `sepBy` char '\n'

dist :: Point -> Point -> Int
dist (x, y) (a, b) = abs (x - a) + abs (y - b)

type Interval = (Int, Int)

data Sensor = Sensor {pos :: Point, bpos :: Point, mandist :: Int}
 deriving Show

posX :: Sensor -> Int
posX = fst . pos

posY :: Sensor -> Int
posY = snd . pos

solve :: Int -> [Sensor] -> [Interval]
solve y sensors =  combine $ sort 
  [ buildInterval (posX s) (mandist s - sdist)
  | s <- sensors
  , let sdist  = abs (y - posY s)
  , sdist <= mandist s
  ]

solve1 :: Int -> [Sensor] -> Int
solve1 y sensors = sumIntervals $ splitIntervals y sensors $ solve y sensors

buildInterval :: Int -> Int -> Interval
buildInterval cp d = (cp - d, cp + d)

combine :: [Interval] -> [Interval]
combine ((i, j):(a, b):xs)
  | j + 1 == a = combine ((i, b):xs)
  | a <= j    = combine ((i, max j b):xs)
  | otherwise = (i, j):combine ((a, b):xs)
combine is = is

sumIntervals :: [Interval] -> Int
sumIntervals = sum . map (\(i, j) -> succ (j - i))

splitIntervals :: Int -> [Sensor] -> [Interval] -> [Interval]
splitIntervals y sens ivals = 
  let xs = sort $ map (fst . bpos) $ filter ((y ==) . snd . bpos) sens
  in splitcomb xs ivals

-- invariant xs sorted
splitcomb :: [Int] -> [Interval] -> [Interval]
splitcomb (x:xs) is@((a,b):r) | x > b     = (a,b) : splitcomb (x:xs) r
                              | x < a     = splitcomb xs is
                              | otherwise = splitcomb xs ((a, x-1) : (x+1, b) : r)
splitcomb _      is           = is


solve2 :: Interval -> [Sensor] -> Int
solve2 (i, j) sensors = let ivals = zip [i..j] $ map (cutIntervals (i, j) . (`solve` sensors)) [i..j]
                            -- safe if there is a solution
                            ((y, ival):_) = filter ((> 1) . length . snd) ivals
                            x = unfoldIval 0 ival
                        in x * 4000000 + y

cutIntervals :: Interval -> [Interval] -> [Interval]
cutIntervals (i, j) = map (bimap (max i) (min j))

unfoldIval :: Int -> [Interval] -> Int
unfoldIval n []             = n
unfoldIval n ((a, b):ivals) | n < a     = n
                            | otherwise = unfoldIval (b+1) ivals