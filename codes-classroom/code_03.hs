{-- Higher Order Functions --}

-- Função que recebe uma função fnc de dois argumentos 
-- como parametro e dois parametro , a e b, inteiros e 
-- retorna a aplicaçõa da função fnc em a e b 
res :: (t1 -> t2 -> t3) -> t1 -> t2 -> t3
res fnc a b = fnc a b;

o' :: (t -> t -> t) -> [t] -> t
o' fnc [] = error "empty list"
o' fnc [a] = a
o' fnc (x:xs) = fnc x (o' fnc xs)   

ls  = zip "abc" [1..3]

ls' = zip "ab" [1..3]

ls'' = map (\x -> x^2) [1..10] 