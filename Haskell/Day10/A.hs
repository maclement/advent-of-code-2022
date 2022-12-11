import Data.Maybe (fromJust)
import Parser
import Control.Applicative
import Data.Bool (bool)
import Data.Bifunctor
import Data.Functor

data Instruction = Noop | Addx Int 
  deriving Show

main :: IO ()
main = input solve1 >> picture

input :: Show a => ([Instruction] -> a) -> IO ()
input f  = readFile ".\\input.txt"  >>= print . f . fromJust . runParser (some parseInstruction)

picture :: IO ()
picture = readFile ".\\input.txt" >>= mapM putStrLn . solve2 . fromJust . runParser (some parseInstruction) >> return ()

parseInstruction :: Parser Instruction
parseInstruction = ((string "noop" $> Noop) 
     <|> ( Addx <$> (string "addx" *> many space  *> parseInt))) <* many (char '\n')

solve1 :: [Instruction] -> Int
solve1 instr = sum [ (simulateInstr 1 instr !! (c-1)) * c | c <- cycles ]

simulateInstr :: Int -> [Instruction] -> [Int] 
simulateInstr lastValue []     = []
simulateInstr lastValue (x:xs) = case x of
  Noop   -> lastValue : simulateInstr lastValue xs
  Addx i -> lastValue : lastValue : simulateInstr (lastValue + i) xs

cycles :: [Int]
cycles = [20, 60 .. 220]

solve2 :: ([Instruction] -> [String])
solve2 instr = map convert $ splitCycles (simulateInstr 1 instr)

splitCycles :: [Int]  -> [[Bool]]
splitCycles []    = []
splitCycles instr = let (input, remainder) = splitAt 40 instr
                    in (map inInterval $ zip [0..39] input) : splitCycles remainder

inInterval :: (Int, Int) -> Bool
inInterval = \(drawPos, spriteMiddle) -> drawPos `elem` [spriteMiddle - 1..spriteMiddle + 1]

convert :: [Bool] -> String
convert = map (bool '.' '#')