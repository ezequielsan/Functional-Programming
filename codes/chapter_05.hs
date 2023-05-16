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

replicate' :: (Ord i, Num i) => i -> a -> [a]
replicate' n x 
    | n <= 0 = []
    | otherwise = x:replicate' (n-1) x