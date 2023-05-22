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

elemento :: (Eq t, Num t) => [a] -> t -> a
elemento u 0 = head u
elemento u n = elemento (tail u) (n-1) 

pertence :: (Eq a) => [a] -> a -> Bool
pertence u n =  not (null [ x | x <- u, x == n ]) 

total :: (Num b) => [a] -> b
total [] = 0
total (x:xs) = 1 + total xs 

maior :: (Ord a) => [a] -> a
maior [] = error "Empty list, not exists max element"
maior [x] = x
maior (x:xs)  
    | x >= maxTail = x
    | otherwise = maxTail
    where maxTail = maior xs

frequencia :: (Ord a) => [a] -> a -> Int
frequencia [] u = 0
frequencia xs u = length ls
    where ls = [ n | n <- xs, n == u ]

unico :: (Ord a) => [a] -> a -> Bool
unico xs x = frequencia xs x == 1

maioresQue :: (Ord a) => a -> [a] -> [a]
maioresQue x xs = [ n | n <- xs, n > x ]

concat :: [a] -> [a] -> [a]
concat xs [] = xs
concat xs (s:ls) =  