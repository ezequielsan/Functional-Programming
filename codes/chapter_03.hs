{- Types and Typeclasses in Haskell -}

-- Definindo os tipos de funções
removeNonUppercase :: [Char] -> [Char] 
removeNonUppercase str = [ c | c <- str, c `elem` ['A'..'Z'] ]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n = product [2..n]

-- Ambos os tipos Int e Integer reprsentam o tipo inteiro, o
-- que muda é a faixa de valores que eles podem representar
-- a faixa do Integer é muito maior.

circumference  :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

{- Chapter exercises -}
-- 1.  Faça uma função que remove os espaços existentes no
--     início e final de uma string dada.
strip :: [Char] -> [Char]
strip "" = ""
strip all@(x:xs) 
    | x == ' ' = strip xs
    | head reverseOfList == ' ' = reverse (strip (tail reverseOfList ))
    | otherwise = x:xs 
    where reverseOfList = reverse all

-- 2. Faça um função que dada uma frase em string, separe a primeira
--    palavra do restante da string.
--    Exemplo: popWord "casa de tijolos" -> ("casa", "de tijolos")
popWord :: [Char] -> ([Char], [Char])
popWord all@(x:xs) 
    | strip all == "" = error "Empty string"
    | otherwise = (takeWhile (/= ' ') all, strip (dropWhile (/= ' ') all))

popWord' :: [Char] -> ([Char], [Char])
popWord' ls = (a, strip b)
    where (a, b) = span (/= ' ') ls

