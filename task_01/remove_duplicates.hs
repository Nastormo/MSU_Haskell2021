elemExist:: (Eq) a => [a] -> a -> Bool
elemExist [] el = False
elemExist (x:xs) el
    | x == el = True
    | otherwise = elemExist xs el

getElemsNotDuplicate:: (Eq) a => [a] -> [a]
getElemsNotDuplicate [] = []
getElemsNotDuplicate (x:xs)
    | elemExist xs x = getElemsNotDuplicate xs
    | otherwise = x : getElemsNotDuplicate xs


main = do 
    print (getElemsNotDuplicate [1, 5, 2, 3, 2, 3, 4])