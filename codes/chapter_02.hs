
{- Functions in Haskell -}

doubleMe x = x + x


doubleSmallNumber x = if x > 100
                        then x
                        else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

-- Função sem nenhum parâmetro (definição)
name = "Ezequiel"

{- Lists in Haskell -}

lostNumbers = [4, 7, 2, 9, 6, 9, 0]
name' = ['E', 'z', 'e', 'q', 'u', 'i', 'e', 'l']

-- concat two lits
numbers = [1, 3, 5, 7] ++ [2, 4, 6, 8]
firstName = "Ezequiel"
lastName = "Santos"
completeName = firstName ++ " " ++ lastName;

-- add an element to a list

numbers' = 0:numbers

-- indexing ins Haskell

indexSInName = completeName !! 9 

-- Lits of lists
b = [[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]

-- Texas Ranges
list' = [1..20]
alphabet = ['a'..'z']

-- adding a step
listEvenNumbers = [0, 2 ..20]
listOddNumbers = [1, 3 ..20]


descendingOrder = [20, 19 ..1]

-- Infinite list
list'' = [1..]

-- Examples
-- 1. faça uma função que retorne os n primeiros multiplos de 13
multiples n = take n [13, 26 ..] 

{- List compreension -}
firstTenEvensNumbers = [ 2*x | x <- [1..10], x*2 >= 12] 

list''' = [x | x <- [50..100], x `mod` 7 == 3]

-- Examples
-- 1. Faça uma função que dada uma lista xs substitua cada 
--    impar maior que 10 por "BANG!" e cada impar menor que 
--    10 por "BOOM!". Se o numero não for impar, remova-o.
boomBangs xs = [ if x > 10 then "BANG!" else "BOMM!" | x <- xs, odd x]

-- 2. Faça uma função que retorne todos os numeros inteiros de 
--    10 a 20 que não sejam 13, 15 e 19
filterList = [ x | x <- [10..20], x /= 13, x /= 15, x /= 19 ]

-- Podemos extrair varias listas em uma list compreension
allProducts = [ x*y | x <- [2, 5, 10], y <- [3, 7, 8], x*y > 50]

nouns = ["gerente","programador","cliente"]  
adjectives = ["malemolente","chato","fofoqueiro"] 

combination = [ noun ++ " " ++ adjective | noun <- nouns, adjective <- adjectives ]

-- creating my own length
length' xs = sum [ 1 | _ <- xs ]

removeNonUppercase str = [ c | c <- str, c `elem` ['A'..'Z'] ]

xCoordinate pair = fst pair
yCoordinate pair = snd pair

pairs list1 list2 = zip list1 list2 

-- Problem
-- 1. Que triângulo retângulo que tem números inteiros para 
-- todos os lados e todos os lados iguais ou menores que 10 
-- e tem um perímetro de 24?
triangles = [ (a, b, c) | a <- [1..10], b <- [1..10], c <- [1..10], a^2 + b^2 == c^2, a + b + c == 24 ]

{- Chapter exercises -}
-- 1. Sejam as listas L e L' tal que L' equivale a L sem o primeiro 
-- e último elementos. Faça uma função em haskell que receba L e 
-- retorne L'.  
generateSublist xs = take (length xs - 1) xs    

-- 2. Faça uma função que retorne os n primeniros
--    numero primos

divisores :: Int -> [Int]
divisores x = [ n | n <- [2..x-1], x `mod` n == 0 ]

ehprimo :: Int -> Bool
ehprimo x = if x < 2 then False else null (divisores x)

primos :: Int -> [Int]
primos n = take n ([ x | x <- [1..], ehprimo x])  