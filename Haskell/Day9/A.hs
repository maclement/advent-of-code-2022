import Data.List (nub)
import Parser
import Control.Applicative
import Data.Maybe (isJust, fromJust)
import Data.Bifunctor (bimap, first, second)
import Control.Monad.Writer

data Dir = R | U | L | D
 deriving (Read, Show)

type Pos = (Int, Int)

main :: IO ()
main = input solve1 >> input solve2

input :: Show a => ([(Dir, Int)] -> a) -> IO ()
input f  = readFile ".\\input.txt"  >>= print . f . fromJust . runParser (many parseMove)

parseMove :: Parser (Dir, Int)
parseMove = curry (bimap (read . return) read) <$> asum (char <$> "RULD") <* many space <*> some digit <* many (char '\n')

solve1 :: ([(Dir, Int)] -> Int)
solve1 dirs = length $ nub $ execWriter $ move (0,0) (0,0) dirs

solve2 :: ([(Dir, Int)] -> Int)
solve2 dirs = length $ nub $ execWriter $ move10 (replicate 10 (0,0)) dirs

move :: Pos -> Pos -> [(Dir, Int)] -> Writer [Pos] ()
move ph       pt []          = return ()
move ph       pt ((_, 0):ms) = move ph pt ms
move ph       pt ((d, n):ms) = do 
  let ph' = dir d ph
  let pt' = tailMove ph' pt
  tell [pt']
  move ph' pt' ((d, n-1):ms)
      
tailMove :: Pos -> Pos -> Pos
tailMove h@(hx, hy) t@(tx, ty) 
  | hx == tx + 2 && hy == ty = (succ tx,ty)
  | hx == tx - 2 && hy == ty = (pred tx,ty)
  | hy == ty + 2 && hx == tx = (tx, succ ty)
  | hy == ty - 2 && hx == tx = (tx, pred ty)
  | hx /= tx && hy /= ty && dist h t > 2 = let possible = [ (x', y') 
                                                          | x' <- [pred tx .. succ tx]
                                                          , y' <- [pred ty .. succ ty]
                                                          , dist t (x', y') == 2
                                                          ]
                                           in snd $ minimum [ (dist h t', t')  | t' <- possible ]
  | otherwise = t

dist :: Pos -> Pos -> Int
dist (hx, hy) (tx, ty) = abs (hx - tx) + abs (hy - ty)

dir :: Dir -> Pos -> Pos
dir R = second succ
dir U = first succ
dir L = second pred
dir D = first pred

move10 :: [Pos] -> [(Dir, Int)] -> Writer [Pos] ()
move10 (p:ps) []  = return ()
move10 ps     ((_, 0):ms) = move10 ps ms
move10 (p:ps) ((d, n):ms) = do
    let p' = dir d p
    let (ps', lastPos) = simulateAll p' ps
    tell [lastPos]
    move10 (p':ps') ((d, pred n):ms)

simulateAll :: Pos -> [Pos] -> ([Pos], Pos) 
simulateAll predPos []     = ([], predPos)
simulateAll predPos (p:ps) = let p'             = tailMove predPos p
                                 (ps', lastPos) = simulateAll p' ps
                             in (p':ps', lastPos)