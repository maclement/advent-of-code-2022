import Data.Maybe (fromJust)
import Parser
import Control.Applicative
import Data.Bool (bool)
import Data.Bifunctor
import Data.Functor
import Control.Monad.State
import Data.List (sortBy)

data Monkey = Monkey { number :: Int, items :: [Int], op :: Int -> Int, nextMonkey :: (Int, Int -> Int) }

main :: IO ()
main = input (solve 20 (`div` 3)) >> input (solve2 10000)

input :: Show a => ([Monkey] -> a) -> IO ()
input f  = readFile ".\\input.txt"  >>= print . f . fromJust . runParser (some parseMonke)

parseMonke :: Parser Monkey
parseMonke = do
  n <- parseNumber <* many (char ' ')
  items <- parseItems <* many (char ' ')
  o <- parseOp <* many (char ' ')
  next <- parseNextMonkey <* many (char '\n')
  return (Monkey n items o next)

parseNumber :: Parser Int
parseNumber = string "Monkey " *> parseInt <* string ":\n"

parseItems :: Parser [Int]
parseItems = string "Starting items: " *> some (parseInt <* many (string ", ")) <* char '\n'

parseOp :: Parser (Int -> Int)
parseOp = (string "Operation: new = old " *> (((*) <$ string "* "  <|> (+) <$ string "+ ") <*> parseInt)
       <|> string "Operation: new = old * old" *> pure (^2))               
       <* char '\n'

parseNextMonkey :: Parser (Int, Int -> Int)
parseNextMonkey = do n <- parseTest <* char '\n'
                     many (char ' ')
                     t <- parseTrue <* char '\n'
                     many (char ' ')
                     f <- parseFalse <* many (char '\n')
                     return (n, \x -> bool f t (x `mod` n == 0))
 where
  parseTest :: Parser Int
  parseTest = string "Test: divisible by " *> parseInt
  
  parseTrue :: Parser Int
  parseTrue = string "If true: throw to monkey " *> parseInt
  
  parseFalse :: Parser Int
  parseFalse = string "If false: throw to monkey " *> parseInt

simulateRounds :: [Monkey]  -> Int -> [Monkey]
simulateRounds queue  0 = queue
simulateRounds (Monkey _ it o next : qs) n = undefined
simulateRounds [] _ = error "Monke got lost"

solve :: Int -> (Int -> Int) -> [Monkey] -> Int
solve i f monkes = product $ take 2 $ sortBy (flip compare) (evalState (simRounds f (i * length monkes)) (zip (repeat 0) monkes))

simRounds :: (Int -> Int) -> Int -> State [(Int, Monkey)] [Int]
simRounds _ 0 = gets $ map fst
simRounds f n = do
  (active, (Monkey num it op next)) <- gets head
  let it' = map (f . op) it
  modify (\s -> tail s ++ [(active + length it, Monkey num [] op next)])
  sequence $ zipWith performUpdate it' (map (snd next) it')
  simRounds f (n-1)

performUpdate :: Int -> Int -> State [(Int, Monkey)] ()
performUpdate item to =
  modify $ map $ second (\m -> if number m == to then m{items = items m ++ [item]} else m)

solve2 :: Int -> [Monkey] -> Int
solve2 i monkes = let p = product $ map (fst . nextMonkey) monkes
  in product $ take 2 $ sortBy (flip compare) (evalState (simRounds (`mod` p) (i * length monkes)) (zip (repeat 0) monkes))

