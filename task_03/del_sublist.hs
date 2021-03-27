del_sublist' :: (Ord) a => ([a], [a], [a], [a]) -> [a]
del_sublist' (xs, [], zs, ws) = del_sublist' (zs ++ xs, ws, [], [])
del_sublist' ([], ys, zs, ws) = zs ++ ws
del_sublist' ((x:xs), (y:ys), zs, ws)
    | x == y = del_sublist' (xs, ys, zs, ws ++ [y])
    | otherwise = del_sublist' (xs, (ws ++ [y] ++ ys), zs ++ [x], [])


del_sublist :: (Ord) a => ([a], [a]) -> [a]
del_sublist (xs, ys) = del_sublist'(xs, ys, [], [])


main = do
    print (del_sublist'([], [1, 2, 3], [4, 5], [6, 7]))
    print (del_sublist'([1, 2, 3, 4, 5], [], [3], [2, 3]))
    print (del_sublist ([1, 2, 3, 4, 5], [3, 4]))