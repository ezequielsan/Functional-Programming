-- Funções lambdas
map' f xs = [f n | n <- xs]

-- Folders
sum' = foldr (+) 0 [1..10]
