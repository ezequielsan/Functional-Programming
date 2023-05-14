-- IDENTIFICAÇÃO
matricula = "521431" -- coloque a matricula aqui entre as asspas

-- Nome 
nome = "Ezequiel dos Santos Melo" -- coloque seu nome aqui entre aspas

-- ATIVIDADE 3

-- Remove espaços existentes no início
-- e final de uma strings dada.

strip :: [Char] -> [Char]
strip str
    |  null str = ""
    |  head str == ' ' && last str /= ' ' = strip (tail str)
    |  head str /= ' ' && last str == ' ' = strip (take (length str - 1) str )
    |  head str == ' ' && last str == ' ' = strip (take (length str - 1) (tail str))
    | otherwise = str

-- Separa a primeira palavra do restante
-- de uma string (Palavras são substeings 
-- separadas por espaços). Exemplo,

-- >> popWord "casa  de tijolos"
-- ["casa", "de tijolos"]'
-- >>

popWord :: [Char] -> ([Char], [Char])
popWord xs = 
    (strip (fst (break (== ' ') (strip xs))), strip (snd (break (== ' ') (strip xs))))


-- Processa uma string e retorna 
-- a lista de suas palavras. OBS: 
-- palavras não devem ter espaços 
-- extemos e nem serem vazias. Exemplo,

-- >> splitStr " The   fox jumped  "
-- ["The", "fox", "jumped"]

splitStr :: [Char] -> [ [Char] ]
splitStr xs
    |  null (snd (popWord (strip xs))) = [fst (popWord (strip xs))]
    |  otherwise = [fst (popWord (strip xs))] ++ splitStr (snd (popWord (strip xs)))