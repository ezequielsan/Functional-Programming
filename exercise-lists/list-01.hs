lesserOfTwo :: (Ord a) => a -> a -> a
lesserOfTwo x y = min x y

lesserOfThree :: (Ord a) => a -> a -> a -> a
lesserOfThree x y z = min (min x y) z

fatorial ::  (Integral a) => a -> a
fatorial n = if n == 1 then 1 else n * fatorial (n-1)

fibonacci :: (Integral a) => a -> a
fibonacci 1 = 0
fibonacci 2 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2) 

elemento u 0 = head u
elemento u n = elemento (tail u) (n-1) 

pertence :: (Eq a) => [a] -> a -> Bool
pertence u n =  not (null [ x | x <- u, x == n ]) 

total :: (Num b) => [a] -> b
total [] = 0
total (x:xs) = 1 + total xs 