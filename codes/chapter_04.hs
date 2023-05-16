{- Sintaxe in Functions -}

-- Patern Matching
-- consiste na pesquisa por padrões em determinados dados
-- e caso ternha sucesso fazer algo com ele.

lucky :: (Integral a) => a -> String
lucky 7 = "SETE! BINGO"
lucky x = "Desculpe, tente novamente"

sayMe :: (Integral a) => a -> String
sayMe 1 = "Um"
sayMe 2 = "Dois"
sayMe 3 = "Tres"
sayMe 4 = "Quatro"
sayMe 5 = "Cinco"
sayMe x = "Nao esta entre 1 e 5"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- OBS: A ordem é muito importante ao definir um pattern matching
--      e é melhor colocar os padrões mais especificos no inicio

-- Caso de erro
charName :: Char -> String
charName 'a' = "Ana"
charName 'b' = "Bianca"
charName 'c' = "Cecilia"

-- Problem 1: Crie uma função que receba dois vetores em tupla e some-os
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

-- Usando Partern Matching no Problem 1
addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

-- Usando Partern Matching em compreension lists
tupleInList :: Num a => [(a, a)] -> [a]
tupleInList xs = [ a + b | (a,b) <- xs ]

head' :: [a] -> a
head' [] = error "Empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "Empty list"
tell [x] = "The list has only one element: " ++ show x  
tell [x,y] = "The list has three elements: " ++ show x ++ " e " ++ show y
tell (x:y:_) = "The list is too long. Look at the first two elements: " ++ show x ++ " e " ++ show y

-- Problem 2: Implement a função length usando pattern matching
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- as patterns
capital :: String -> String
capital "" = "Lista vazia"
capital all@(x:_) = "A primeira letra de " ++ all ++ " eh " ++ [x] 

-- Guards
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | weight / (height ^ 2) < 18.5 = "Voce esta abaixo do peso"
    | weight / (height ^ 2) <= 25.0 = "Suspostamente voce esta normal. Pff, aposto que voce e feio" 
    | weight / (height ^ 2) <= 30.0 = "Voce esta gordo! Faca um exercicio gorducho"
    | otherwise = "Voce eh um baleia, meus parabens"  

max' :: (Ord a) => a -> a -> a
max' a b 
    | a > b = a
    | otherwise = b  

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b 
    | a > b = GT
    | a == b = EQ
    | otherwise = LT

-- Where
bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
    | bmi < 18.5 = "Voce esta abaixo do peso"
    | bmi <= 25.0 = "Suspostamente voce esta normal. Pff, aposto que voce e feio" 
    | bmi <= 30.0 = "Voce esta gordo! Faca um exercicio gorducho"
    | otherwise = "Voce eh um baleia, meus parabens" 
    where bmi = weight / (height ^ 2) 

bmiTell'' :: (RealFloat a) => a -> a -> String
bmiTell'' weight height
    | bmi < shinny = "Voce esta abaixo do peso"
    | bmi <= normal = "Suspostamente voce esta normal. Pff, aposto que voce e feio" 
    | bmi <= fat = "Voce esta gordo! Faca um exercicio gorducho"
    | otherwise = "Voce eh um baleia, meus parabens" 
    where bmi = weight / (height ^ 2) 
          shinny = 18.5
          normal = 25.0
          fat = 30.0

-- OBS: where ser para definimos "variaveis ou funções" que usaremos 
--      dentro da nossa função, evitando repetições

-- where com pattern matching
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

-- Além de definir constantes par where, também podemos definir funções
calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [ bmi w h | (w, h) <- xs ]
    where bmi weight height = weight / (height ^ 2)

-- Let it be
-- São associações muito semelhantes as where, porem mais restritas

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
    let sideArea = 2 * pi * r * h
        topArea = pi * r^2
    in sideArea + 2 * topArea

-- podemos usar let in em qualquer lugar
sumCrazy = 4 * (let a = 9 in a + 1) + 2

squareCrazy = [ let square x = x * x in (square 5, square 3, square 2 ) ]

tupleCrazy = (let (a,b,c) = (1, 2, 3) in a + b + c) * 100

-- Case expressions
-- São estruturas muito parecidas com os cases das linguagens 
-- imperativas

-- Podemos reescrever a função head ja apresentada usando case
head'' :: [a] -> a
head'' xs = case xs of [] -> error "Not exists, empty  list"
                       (x:_) -> x

{- Sintaxe de expressões case 
   case expression of pattern -> result  
                   pattern -> result  
                   pattern -> result  
                   ... 
-}

-- Ao contrário do pattern matching, expressões cases podem 
-- ser colocadas em qualquer lugar
describeList :: [a] -> String
describeList xs = "A lista eh " ++ case xs of [] -> "vazia."
                                              [x] -> "uma lista unitaria."
                                              xs -> "uma lista grande."