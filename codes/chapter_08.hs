import Control.Arrow (Arrow(first))
{- Create Types and Typeclasses -}

-- palavra chave data cria um novo tipo
-- data Bool = False | True

-- definição do tipo Int
-- data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647  

data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

-- Função surface recebe um form geométriva e retorna a 
-- área da superficie dessa forma
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

mapCircles = map (Circle 10 20) [4, 5, 6]

-- Melhorando nosso tipo Shape
data Point = Point Float Float deriving (Show)
data Shape' = Circle' Point Float | Rectangle' Point Point deriving (Show)

surface' (Circle' _ r) = pi * r ^ 2
surface' (Rectangle' (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- Função nudge recebe um Shape e a quantidade a se mover no eixo x e no eixo y e aplica a translação
nudge :: Shape' -> Float -> Float -> Shape'
nudge (Circle' (Point x y) r) a b = Circle' (Point (x + a) (x + b)) r 
nudge (Rectangle' (Point x1 y1) (Point x2 y2)) a b = Rectangle' (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

-- Criando funções auxiliares para não trabalharmos com ponto
baseCircle :: Float -> Shape'
baseCircle r = Circle' (Point 0 0) r

baseRect :: Float -> Float -> Shape'
baseRect width height = Rectangle' (Point 0 0) (Point width height)

-- tipo de dados que descreve uma pessoa
-- Dados da Pessoa: nome, sobrenome, idade, altura, 
-- número do telefone e sabor favorito de sorvete
data Person = Person String String Int Float String String deriving (Show)

guy = Person "Buddy" "Finkslentein" 43 184.5 "527-999" "Chocolate"

-- Funções para acessar os atributos da pessoa
firstName :: Person -> String
firstName (Person firstname _ _ _ _ _) = firstname 

lastName :: Person -> String
lastName (Person _ lastname _ _ _ _) = lastname

age :: Person -> Int
age (Person _ _ age _ _ _) = age

height :: Person -> Float
height (Person _ _ _ height _ _) = height

phoneNumber :: Person -> String
phoneNumber (Person _ _ _ _ phone _) = phone

flavor :: Person -> String
flavor (Person _ _ _ _ _ flavor) = flavor

-- Podemos deixar isso melhor e mais facil usando sintaxe de registro
data Person' = Person' { firstName' :: String,
                         lastName' :: String,
                         age' :: Int,
                         height' :: Float,
                         phoneNumber' :: String,
                         flavor' :: String
                       } deriving (Show)


-- Na sisntaxe de registro o show mostrado é diferente
data Car = Car String String Int deriving (Show)
data Car' = Car' { company :: String, model :: String, year :: Int } deriving (Show)






