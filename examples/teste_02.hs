unique :: Eq a => [a] -> [a]
unique s 
    | null s = []
    | otherwise = x : unique list
    where  x = head s
           xs = tail s
           list = [a | a <- xs, a /= x]

delete'min :: (Ord a) => [a] -> [a]
delete'min [] = []
delete'min [x] = []
delete'min (x:xs) 
    | x < minimum xs = xs
    | otherwise = x : delete'min xs
    
