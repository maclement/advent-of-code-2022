import Parser
import Control.Applicative
import Data.Bifunctor
import Data.Functor
import Data.Maybe (fromJust, listToMaybe)
import Data.List

data Rose a = Leaf a | Rose [Rose a]
 deriving (Eq, Show)

instance Ord a => Ord (Rose a) where
    Leaf x  `compare` Leaf y  = compare x y
    Rose xs `compare` Rose ys = case (xs, ys) of
        ([], []) -> EQ
        ([], _ ) -> LT
        (_ , []) -> GT
        _        -> maybe (compare (length xs) (length ys)) id $ listToMaybe $ filter (/= EQ) $ zipWith compare xs ys
    x@(Leaf _) `compare` y@(Rose _) = Rose [x] `compare` y
    x@(Rose _) `compare` y@(Leaf _) = x `compare` Rose [y]

main :: IO ()
main = input solve1 >> input solve2

input :: Show a => ([(Rose Int, Rose Int)] -> a) -> IO ()
input f = readFile ".\\input.txt"  >>= print . f . fromJust . runParser (some parseRoses)

parseRoses :: Parser (Rose Int, Rose Int)
parseRoses = (,) <$> (rose <* char '\n') <*> (rose <* (many (char '\n'))) <* many (char '\n')

rose :: Parser (Rose Int)
rose = Rose <$> (char '[' *> many ((rose <|> leaf) <* many (char ',')) <* char ']')

leaf :: Parser (Rose Int)
leaf = Leaf <$> parseInt

solve1 :: [(Rose Int, Rose Int)] -> Int
solve1 = sum . map fst . filter ( (/= GT) . snd) . zip [1..] . map (uncurry compare)

deviser1, deviser2 :: Rose Int
deviser1 = fromJust $ runParser rose "[[2]]"
deviser2 = fromJust $ runParser rose "[[6]]"

solve2 :: [(Rose Int, Rose Int)] -> Int
solve2 roses = let (l1, l2) = unzip roses
                   roses'   = sort $ deviser1 : deviser2 : l1 ++ l2
                   Just  p1 = elemIndex1 deviser1 roses'
                   Just  p2 = elemIndex1 deviser2 roses'
               in p1 * p2

elemIndex1 :: Eq a => a -> [a] -> Maybe Int
elemIndex1 = go 1
 where
  go _ _ []     = Nothing
  go i x (y:ys) | x == y    = Just i
                | otherwise = go (i+1) x ys 