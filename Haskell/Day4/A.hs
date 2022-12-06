import Data.List (partition)
import Data.Bool (bool)

main :: IO ()
main = input solve1 >> input solve2

input :: Show a => ([((Int, Int),(Int, Int))] -> a) -> IO ()
input f  = readFile ".\\input.txt"  >>= print . f . map ((pmap (pmap read. splitOn (=='-'))) . splitOn (==',')) . lines

splitOn :: Eq a => (a -> Bool) -> [a] -> ([a], [a])
splitOn = go []
 where
  go :: [a] -> (a -> Bool) -> [a] -> ([a] , [a])
  go acc f []     = (acc, [])
  go acc f (y:ys) | f y       = (acc, ys)
                  | otherwise = go (acc ++ [y]) f ys

pmap :: (a -> b) -> (a, a) -> (b, b)
pmap f (x, y) = (f x, f y)

solve1 :: [((Int, Int),(Int, Int))] -> Int
solve1 = sum . map (bool 0 1  . uncurry isContained)

solve2 :: [((Int, Int),(Int, Int))] -> Int
solve2 = sum . map (bool 1 0 . uncurry dontIntersect)

isContained :: (Int, Int) -> (Int, Int) -> Bool
isContained (x1, y1) (x2, y2) = x1 >= x2 && y1 <= y2 || x1 <= x2 && y1 >= y2

dontIntersect :: (Int, Int) -> (Int, Int) -> Bool
dontIntersect (x1, y1) (x2, y2) = y1 < x2 || y2 < x1