elemExist:: (Eq) a => [a] -> a -> Bool
elemExist [] el = False
elemExist (x:xs) el
    | x == el = True
    | otherwise = elemExist xs el

removeElem:: (Eq) a => [a] -> a -> [a]
removeElem [] el = []
removeElem (x:xs) el
    | x == el = removeElem xs el
    | otherwise = x : removeElem xs el

getElemsUnique:: (Eq) a => [a] -> [a]
getElemsUnique [] = []
getElemsUnique (x:xs)
    | elemExist xs x = getElemsUnique (removeElem xs x)
    | otherwise = x : getElemsUnique xs


main = do
    print (getElemsUnique [1, 5, 2, 3, 2, 3, 4])