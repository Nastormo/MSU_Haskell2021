highEl :: (Ord) a => (Bool, a, [a], [[a]], [[a]]) -> (a, [[a]])
highEl (True, x, xs, xss, []) = (x, xs:xss)
highEl (found, x, xs, xss, ([]:yss)) = highEl (found, x, xs, []:xss, yss)
highEl (False, _, _, _, ((x:xs):xss)) = highEl (True, x, xs, [], xss)
highEl (True, x, xs, xss, ((y:ys):yss))
    | x > y = highEl (True, y, ys, ((x:xs):xss), yss)
    | otherwise = highEl (True, x, xs, (y:ys):xss, yss)

takePiece :: Ord a => (Int, Int, [a]) -> [a]
takePiece (x, y, zs) = take y $ drop x zs

split :: (Ord) a => [a] -> ([a], [a], [a], [a])
split xs = (
    takePiece (0, l, xs),
    takePiece (l, l, xs),
    takePiece (l * 2, l, xs),
    takePiece (l * 3, l, xs)
    ) where l = (2 + length xs) `div` 4

merge :: (Ord) a => [[a]] -> [a]
merge [] = []
merge ([]:xss) = merge xss
merge ((x:xs):xss) = y : merge yss
    where (y, yss) = highEl (False, x, [], [], (x:xs):xss)

msort :: (Ord) a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge [(msort p1), (msort p2), (msort p3), (msort p4)]
    where (p1, p2, p3, p4) = split xs

main = do
    print (msort [5, 4, 3, 2, 1, 0, 9, 8, 7, 6, 5])
    print (merge [[1, 2, 3], [4, 5, 6], [7], [8], [1, 2, 3, 4, 5]])
    print (highEl (False, 0, [], [], [[1, 2, 3], [4, 5, 6], [7], [8]]))
    print (split [1..9])
    print (split [1..10])
    print (split [1..11])
    print (split [1..12])
    print (split [1..13])
    print (split [1..14])