import qualified Data.Vector as V
import Data.Maybe

calcStat' :: Eq a => ([a], V.Vector a, V.Vector Int) ->  (V.Vector a, V.Vector Int)
calcStat' ([], yv, zv) = (yv, zv)
calcStat' ((x:xs), yv, zv)
    | V.elem x yv = calcStat' (xs, yv, zv V.// [(i, zv V.! i + 1)])
    | otherwise = calcStat' (xs, V.snoc yv x, V.snoc zv 1)
    where i = fromMaybe 0 (V.elemIndex x yv)

calcStat :: Eq a => [a] -> ([a], [Int])
calcStat xs = (V.toList elem, V.toList stat)
    where (elem, stat) = calcStat' (xs, V.empty, V.empty)

crateAllPermutations' :: Eq a => ([a], [Int], [a], [Int]) -> [[a]]
crateAllPermutations' ([], [], [], []) = [[]]
crateAllPermutations' ([], [], zs, ws) = []
crateAllPermutations' ((x:xs), (0:ys), zs, ws) = crateAllPermutations' (xs, ys, zs, ws)
crateAllPermutations' ((x:xs), (y:ys), zs, ws) = (map (x :) (crateAllPermutations' (zs ++ [x] ++ xs, ws ++ [(y-1)] ++ ys, [], []))) ++ crateAllPermutations' (xs, ys, (zs ++ [x]), (ws ++ [y]))

crateAllPermutations :: Eq a => ([a], [Int]) -> [[a]]
crateAllPermutations (xs, ys) = crateAllPermutations' (xs, ys, [], [])

allPermutations :: Eq a => [a] -> [[a]]
allPermutations xs = crateAllPermutations $ calcStat xs



main = do
    print (calcStat [0..9])
    print (calcStat [1, 4, 6, 2, 5, 6, 5, 3, 9, 1, 3, 4, 5, 9, 1, 0, 9])
    print (crateAllPermutations ([1, 2, 3, 4], [1, 1, 1, 0]))
    print (allPermutations [1, 2, 3])
    print (allPermutations [1, 1, 2, 3])