-- ATIVIDADE 
atividade = 6

-- IDENTIFICAÇÃO
matricula = "521431" -- coloque a matricula aqui entre as asspas

-- Nome
nome = "Ezequiel dos Santos Melo" -- coloque seu nome aqui entre aspas

-- 1

-- ATIVIDADES A FAZER
-- Construa funções que

data List a = Empty | Node a (List a) deriving (Show)

-- Mapeia uma  lista noutra. As tais
-- devem possuir tipo List.  A função de
-- mapeamento é o primeiro argumento.
--A lista a processar é o segundo.
-- Não modifique a assinatura.

listMap :: (t -> a) -> List t -> List a
listMap _ Empty = Empty
listMap f (Node b xs) = Node (f b) (listMap f xs) 

-- exemplo
-- >> let x = Node 5 (Node 3 (Node 7 (Node 1 (Node 9 Empty))))
-- >>  listMap (\n->n+1) x
-- Node 6 (Node 4 (Node 8 (Node 2 (Node 10 Empty)))) 

-- ----------------------
-------------------------

-- Transforma lista List numa string 
-- cujas chaves são separadas por ":".
-- Não modifique a assunatura.
listToStr :: Show a => List a -> [Char]
listToStr Empty = "" -- implemente aqui
listToStr (Node a Empty) = show a
listToStr (Node a xs) = show a ++ ":" ++ listToStr xs

-- exemplo
-- >> let x = Node 5 (Node 3 (Node 7 (Node 1 (Node 9 Empty))))
-- >> listToStr x
-- "5:3:7:1:9"