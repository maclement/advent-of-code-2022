import Data.List

main :: IO ()
main = input solve1 >> input solve2

input :: Show a => ([[Int]] -> a) -> IO ()
input f  = readFile ".\\input.txt"  >>= print . f  . map ( map $ read . (:[]) ) . lines

solve1 :: [[Int]] -> Int
solve1 matrix = length [(i,j) 
                       | i <- [0..length matrix - 1] , j <- [0..length (matrix !! 0) - 1]
                       , checkLeftAndRight (i, j) matrix || checkLeftAndRight (j, i) (transpose matrix)
                       ]

checkLeftAndRight :: (Int, Int) -> [[Int]] -> Bool
checkLeftAndRight (row, col) m = let mRow = m !! row
                                     val  = mRow !! col
                                     (left, _:right) = splitAt col mRow 
                                 in  (null $ filter (>=val) left) || (null $ filter (>=val) right)

test :: [[Int]]
test = [[3,0,3,7,3],[2,5,5,1,2],[6,5,3,3,2],[3,3,5,4,9],[3,5,3,9,0]]

solve2 :: [[Int]] -> Int 
solve2 matrix = maximum [ checkLeftAndRightDist (i,j) matrix * checkLeftAndRightDist (j,i) (transpose matrix)
                        | i <- [0..length matrix - 1] 
                        , j <- [0..length (matrix !! 0) - 1]
                        ]

checkLeftAndRightDist :: (Int, Int) -> [[Int]] -> Int
checkLeftAndRightDist (row, col) m = let mRow = m !! row
                                         val  = mRow !! col
                                         (left, _:right) = splitAt col mRow 
                                         distLeft = length (takeWhile (< val) $ reverse left)
                                         distRight = length (takeWhile (< val) $ right)
                                     in edgeCases (length left) distLeft * edgeCases (length right) distRight

edgeCases :: Int -> Int -> Int
edgeCases len taken | len == 0     = 0
                    | len == taken = taken
                    | otherwise    = taken + 1 
    
computeScenic :: (Int, Int) -> [[Int]] -> Int
computeScenic (row, col) matrix = checkLeftAndRightDist (row, col) matrix * checkLeftAndRightDist (row, col) (transpose matrix)