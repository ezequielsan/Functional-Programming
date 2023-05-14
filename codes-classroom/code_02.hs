colatz :: Integral a => a -> [a]
colatz 1 = [1]
colatz n 
    | (p == 0) = n:colatz x
    | otherwise = n:colatz y
    where p = n `mod` 2
          x = n `div` 2
          y = (3*n + 1) `div` 2

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted
