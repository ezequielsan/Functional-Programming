length' :: (Num b) => [a] -> b  
length' [] = 0  
length' (_:xs) = 1 + length' xs  

strip :: [Char] -> [Char]
strip str
    |  null str = ""
    |  head str == ' ' && last str /= ' ' = strip (tail str)
    |  head str /= ' ' && last str == ' ' = strip (take (length str - 1) str )
    |  head str == ' ' && last str == ' ' = strip (take (length str - 1) (tail str))
    | otherwise = str


popWord :: [Char] -> ([Char], [Char])
popWord xs = 
    (strip (fst (break (== ' ') (strip xs))), strip (snd (break (== ' ') (strip xs))))

-- splitStr :: [Char] -> [ [Char] ]
-- splitStr null (head xs) =  [fst (popWord (strip xs))]
-- splitStr xs = [fst (popWord (strip xs))] ++ splitStr (snd (popWord (strip xs)))

splitStr' :: [Char] -> [ [Char] ]
splitStr' xs
    |  null (snd (popWord (strip xs))) = [fst (popWord (strip xs))]
    |  otherwise = [fst (popWord (strip xs))] ++ splitStr' (snd (popWord (strip xs)))