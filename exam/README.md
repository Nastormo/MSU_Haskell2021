# Задача 5

```
import System.Environment
import System.IO
import Data.List.Split
import Data.Typeable(typeOf)

checkLine :: [Char] -> Int
checkLine xs = checkLine' xs [] 0 0

checkLine' :: [Char] -> [Char] -> Int -> Int -> Int
checkLine' [] [] _ w = w
checkLine' [] ys _ _ = -1
checkLine' (x:xs) [] z w
    | (x == '(') || (x == '[') || (x == '{') || (x == '<') = checkLine' xs [x] (z+1) (max (z+1) w)
    | (x == ')') || (x == ']') || (x == '}') || (x == '>') = -1
    | otherwise = checkLine' xs [] z w
checkLine' (x:xs) (y:ys) z w
    | (x == '(') || (x == '[') || (x == '{') || (x == '<') = checkLine' xs (x:y:ys) (z+1) (max (z+1) w)
    | (x == ')') && (y == '(') = checkLine' xs ys (z-1) w
    | (x == ']') && (y == '[') = checkLine' xs ys (z-1) w
    | (x == '}') && (y == '{') = checkLine' xs ys (z-1) w
    | (x == '>') && (y == '<') = checkLine' xs ys (z-1) w
    | (x == ')') && (y /= '(') = -1
    | (x == ']') && (y /= '[') = -1
    | (x == '}') && (y /= '{') = -1
    | (x == '>') && (y /= '<') = -1
    | otherwise = checkLine' xs (y:ys) z w

main = do
    input <- getLine
    print (checkLine input)
```

Пример работы:
![5.hs](./img/5.png)
