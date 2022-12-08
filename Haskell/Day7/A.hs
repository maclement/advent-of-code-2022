{-# LANGUAGE LambdaCase #-}

import Data.List (groupBy)
import Control.Monad.State
import Data.Char (isDigit)

data FileSystem = Directory { unDirectory :: [FileSystem] } 
                | File { size :: Int } 

foldFileSystem :: ([c] -> c) -> (Int -> c) -> FileSystem -> c
foldFileSystem dir file system = case system of
  Directory files -> dir $ map (foldFileSystem dir file) files
  File i          -> file i

type Command = [String]

main :: IO ()
main = solve task1 >> solve task2

solve :: Show a => (FileSystem -> a) -> IO ()
solve f = readFile ".\\input.txt" >>= print . f . buildFileSystem

buildFileSystem :: String -> FileSystem
buildFileSystem s = evalState buildDirectory (groupBy command (lines s))

command :: String -> String -> Bool
command _       ('$':_) = False
command ('$':_) _       = True
command _       _       = False

buildDirectory:: State [Command] FileSystem
buildDirectory = Directory <$> navigateDirectory []

navigateDirectory :: [FileSystem] -> State [Command] [FileSystem]
navigateDirectory fs = get >>= \case
      []            -> return fs
      ["$ cd .."]:_ -> modify tail >> return fs 
      (c:cs) -> do 
        modify tail
        case c of
          (('$':_:'c':'d':_):_) -> do 
            x <- buildDirectory
            navigateDirectory (x : fs) 
          (_:files)             -> 
            navigateDirectory 
              $ fs ++ [ File (read n) 
                      | l <- files
                      , let n = takeWhile isDigit l
                      , (not . null) n 
                      ]

data AnnotFileSystem = AnnotDirectory { unAnnot :: [AnnotFileSystem], aSize :: Int}
                     | AnnotFile { aSize :: Int }

foldAnnotFileSystem :: ([c] -> Int -> c) -> (Int -> c) -> AnnotFileSystem -> c
foldAnnotFileSystem dir file system = case system of
  AnnotDirectory files i -> dir (map (foldAnnotFileSystem dir file) files) i
  AnnotFile i            -> file i

task1 :: FileSystem -> Int
task1 = sumSmaller 100000 . annot

annot :: FileSystem -> AnnotFileSystem
annot = foldFileSystem (\aFiles -> AnnotDirectory aFiles (sum $ map aSize aFiles)) AnnotFile

sumSmaller :: Int -> AnnotFileSystem -> Int
sumSmaller i (AnnotFile j) = 0
sumSmaller i (AnnotDirectory fs j) 
  | j <= i    = j + sum (map (sumSmaller i) fs)
  | otherwise = sum $ map (sumSmaller i) fs

task2 :: FileSystem -> Int
task2 fs = let required = 30000000 - (70000000 - (aSize $ annot fs))      
           in minimum . filter (>= required) . foldAnnotFileSystem (flip (:) . concat) (const [0]) . annot $ fs