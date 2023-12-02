import Data.Set (Set)
import qualified Data.Set as Set
import Control.Applicative
import Data.Maybe (fromJust)
import Parser
import Control.Monad.State
import Control.Monad

-- Note that the computation took around 40 min so this approach was not very efficient
main :: IO ()
main = input solve1 >> input solve2

input :: Show a => ([(Int, Int)] -> a) -> IO ()
input f = readFile ".\\input.txt"  >>= print . f . concat . fromJust . runParser (some parseStuff)

parsePair :: Parser (Int, Int)
parsePair = (,) <$> (parseInt <* char ',') <*> parseInt

parseStuff :: Parser [(Int, Int)]
parseStuff = ( (\x xs -> concat $ zipWith fromTo (x:xs) xs) <$> parsePair <*> some (string " -> " *> parsePair)) <* many (char '\n')

chain :: [(Int, Int)] -> [(Int, Int)]
chain [x, y]       = fromTo x y
chain (x : y : zs) = fromTo x y ++ chain (y : zs)

fromTo :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
fromTo (i, j) (k, l) | i > k     = fromTo (k, j) (i, l)
                     | j > l     = fromTo (i, l) (k, j)
                     | otherwise = (,) <$> [i .. k] <*> [j .. l]

solve1 :: [(Int, Int)] -> Int
solve1 list = evalState (allSand (maximum $ map snd list)) (Set.fromList list)

allSand :: Int -> State (Set (Int, Int)) Int
allSand maxY = do
    b <- sandFall maxY (500, 0)
    if b then return 0 else succ <$> allSand maxY

sandFall :: Int -> (Int, Int) -> State (Set (Int, Int)) Bool
sandFall maxY (x, y)
  | y > maxY  = return True
  | otherwise = do
    b <- isFree (x, succ y)
    b' <- isFree (pred x, succ y)
    b'' <- isFree (succ x, succ y)
    case (b, b', b'') of
        _ | b   -> sandFall maxY (x, succ y)
          | b'  -> sandFall maxY (pred x, succ y)
          | b'' -> sandFall maxY (succ x, succ y)
        _       -> modify (Set.insert (x, y)) >> return False

isFree :: (Int, Int) -> State (Set (Int, Int)) Bool
isFree p = Set.notMember p <$> get

solve2 :: [(Int, Int)] -> Int
solve2 list = evalState (untilFull (2 + maximum (map snd list))) (Set.fromList list)

untilFull :: Int -> State (Set (Int, Int)) Int
untilFull barrier = do
  b <- isFree (500, 0)
  if b
  then succ <$> do
    sandFall2 barrier (500, 0)
    untilFull barrier
  else return 0

sandFall2 :: Int -> (Int, Int) -> State (Set (Int, Int)) ()
sandFall2 barrier (x, y)
  | succ y >= barrier = modify (Set.insert (x, y))
  | otherwise = do
    b <- isFree (x, succ y)
    b' <- isFree (pred x, succ y)
    b'' <- isFree (succ x, succ y)
    case (b, b', b'') of
        _ | b   -> sandFall2 barrier (x, succ y)
          | b'  -> sandFall2 barrier (pred x, succ y)
          | b'' -> sandFall2 barrier (succ x, succ y)
        _       -> modify (Set.insert (x, y))