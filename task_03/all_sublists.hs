merge :: (Ord) a => ([[a]], [[a]]) -> [[a]]
merge ([], yss) = yss
merge (xss, []) = xss
merge ((xs:xss), (ys:yss))
    | length xs < length ys = xs : merge (xss, ys:yss)
    | otherwise = ys : merge (xs:xss, yss)

sublists :: (Ord) a => (Bool, [a]) -> [[a]]
sublists (_, []) = []
sublists (True, [x]) = [[]] ++ [[x]]
sublists (False, [x]) = [[x]]
sublists (True, xs) =  merge(sublists (False, (init xs)), sublists (True, (tail xs))) ++ [xs]
sublists (False, xs) = sublists (False, (init xs)) ++ [xs]

main = do
    print (merge ([[1], [1, 2], [1, 2, 3]], [[3], [3, 2], [3, 2, 1]]))
    print (sublists (True, [5, 4, 3, 2, 1]))
