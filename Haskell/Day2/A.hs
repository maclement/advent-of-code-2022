import Data.Char (isAlpha)
import Control.Monad (foldM)

newtype Choice = Choice Char
 deriving Show

instance Eq Choice where
    Choice c1 == Choice c2 = (c1, c2) `elem` [('A','X'), ('B', 'Y'), ('C', 'Z')] ++ [ (c,c) | c <- ['X'..'Z']]

-- A Rock, B Paper, C Scissors
-- X Rock, Y Paper, Z Scissors
instance Ord Choice where
    compare (Choice a) (Choice b) 
      | (a, b) `elem` [('A', 'Y'), ('B','Z'), ('C','X')] = LT
      | (a, b) `elem` [('A', 'Z'), ('B','X'), ('C','Y')] = GT
      | (a, b) `elem` zip ['A'..'C'] ['X'..'Z'] = EQ

shapeScore :: [(Choice, Int)]
shapeScore = [(Choice 'X', 1), (Choice 'Y', 2), (Choice 'Z', 3)]

input :: Show a => ([(Choice, Choice)] -> a) -> IO ()
input f  = readFile ".\\input.txt"  
         >>= print . f . map (convert . take 2 . filter isAlpha) . lines
 where
  convert :: [Char] -> (Choice, Choice)
  convert (x : y : _) = (Choice x, Choice y)     
    
main :: IO ()
main = input solve1 >> input solve2

solve1 :: [(Choice, Choice)] -> Maybe Int
solve1 = foldM (fmap . (+)) 0 . map points
 where
  points :: (Choice, Choice) -> Maybe Int
  points (c1, c2) = (+) <$> lookup c2 shapeScore <*> (return $ case c1 `compare` c2 of
    LT -> 6
    EQ -> 3
    GT -> 0)

solve2 :: [(Choice, Choice)] -> Maybe Int
solve2 = foldM (fmap . (+)) 0 . map points
 where
  points :: (Choice, Choice) -> Maybe Int
  points (c1, c2) = (+) 
                  <$> lookup c2 outcomeScore
                  <*> lookup (head [ x | x <- Choice <$> ['X'..'Z'], c1 `compare` x == outcome c2]) shapeScore
                        
outcomeScore :: [(Choice, Int)]
outcomeScore = [(Choice 'X', 0), (Choice 'Y', 3), (Choice 'Z', 6)]

-- GT = Loss, EQ = Draw, LT = Win
outcome :: Choice -> Ordering
outcome (Choice 'X') = GT
outcome (Choice 'Y') = EQ
outcome (Choice 'Z') = LT