{- Recursão -}
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list not exists"
maximum' [x] = x
maximum' (x:xs)
    | x > tailMax = x
    | otherwise = tailMax
    where tailMax = maximum' xs

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "maximum of empty list not exists"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)

replicate'' :: (Num a, Ord a) => a -> b -> [b]
replicate'' n x
    | n <= 0 = []
    | otherwise = x:replicateTail
    where replicateTail = replicate'' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n xs
    | n <= 0 = []
    | null xs = []
    | otherwise = head xs:take' (n-1) (tail xs)

take'' :: (Num i, Ord i) => i -> [a] -> [a]
take'' n _
    | n <= 0 = []
take'' _ [] = []
take'' n (x:xs) = x : take'' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverseTail ++ [x]
    where reverseTail = reverse' xs

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (l:ls) = (x,l):zip' xs ls 

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' n (x:xs) 
    | n == x    = True
    | otherwise = elem' n xs 

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort [ a | a <- xs, a <= x ]
        biggerSorted = quicksort [ a | a <- xs, a > x ]
    in smallerSorted ++ [x] ++ biggerSorted