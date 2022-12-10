{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Applicative
import Control.Monad
import Data.Maybe (listToMaybe)
import Data.Char (isDigit)

newtype Parser a = Parser { unParser :: String -> [(String, a)] }

instance Functor Parser where
    fmap f p = Parser $ map (f <$>) . unParser p

    -- fmap f p = Parser $ \s -> [ (s', f x) | (s', x) <- unParser p s ]       

instance Applicative Parser where
    pure x = Parser $ return . flip (,) x
    p1 <*> p2 = Parser $ \s -> [ (s'', f x) | (s', f) <- unParser p1 s, (s'', x) <- unParser p2 s' ]

instance Alternative Parser where
    empty = Parser $ const [] 
    p1 <|> p2 = Parser $ \s -> unParser p1 s ++ unParser p2 s

instance Monad Parser where
    p1 >>= f = Parser $ \s -> unParser p1 s >>= \(s', a) -> unParser (f a) s'

instance MonadPlus Parser where
    mplus = (<|>)

runParser :: Parser a -> String -> Maybe a
runParser p = fmap snd . listToMaybe . filter ((== "") . fst) . unParser p

char :: Char -> Parser Char
char c = Parser $ \case 
                    []     -> [] 
                    (x:xs) | c == x    -> [(xs, x)]
                           | otherwise -> []

string :: String -> Parser String
string = foldM (\s c -> (:) <$> char c <*> pure s) []

space :: Parser ()
space = char ' ' *> return ()

digit :: Parser Char
digit = Parser $ \case 
                    []     -> [] 
                    (x:xs) | isDigit x -> [(xs, x)]
                           | otherwise -> []