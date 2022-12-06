import Data.Char (isDigit)
import Data.List (groupBy)

main :: IO ()
main = input (solve reverse) >> input (solve id)

input :: Show a => (([String], [String]) -> a) -> IO ()
input f  = readFile ".\\modInput.txt"  >>= print . f . span (not . null) . lines

solve :: (String -> String) -> ([String], [String]) -> String
solve f (stacks, instructions) = map head $ simulate f (map fromInst (drop 1 instructions)) stacks
 where
  fromInst :: String -> (Int, Int, Int)
  fromInst s = let (amount : from : to : _) = filter (not . null) $ map (filter isDigit) $ groupBy (\x y -> isDigit x && isDigit y) s
               in (read amount, read from, read to)

  simulate :: (String -> String) -> [(Int, Int, Int)] -> [String] -> [String]
  simulate _ []                           stacks = stacks
  simulate f ((amount, from, to) : insts) stacks = let (pref, stack : remaining) = splitAt (from - 1) stacks
                                                       (toMove, stack') = splitAt amount stack
                                                       stacks' = pref ++ stack' : remaining
                                                 in simulate f insts $ mapAt (to - 1) ((f toMove) ++) stacks'
mapAt :: Int -> (a -> a) -> [a] -> [a]
mapAt _ _ []     = []
mapAt 0 f (x:xs) = f x : xs
mapAt n f (x:xs) = x : mapAt (n-1) f xs