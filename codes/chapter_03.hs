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