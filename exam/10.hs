import System.Environment
import System.IO
import Data.List.Split
import Data.Typeable(typeOf)

customRead path = do
    contents <- readFile path
    return (map (read::String->Int) (words contents))

merge:: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) 
    | x < y = x : (merge xs (y:ys))
    | otherwise = y : (merge (x:xs) ys)

saveArr path arr = do
    outh <- openFile path WriteMode
    hPrint outh arr
    hClose outh

main = do
    file_paths <- getArgs
    first <- customRead (file_paths !! 0)
    second <- customRead (file_paths !! 1)
    saveArr (file_paths !! 2) (merge first second)