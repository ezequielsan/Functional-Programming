min' (x:xs) = foldl (\acc b -> min acc b) x xs

-- Função que receb uma string e retorna a string
-- somente com as letras maiusculas
removeUpper xs = foldl (\acc b -> if b `elem` ['A'..'Z'] then acc ++ [b] else acc) "" xs