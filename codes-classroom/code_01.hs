{- === GUARDS AND WHERE === -}

-- Faça um função que recebe os coeficiente 
-- a, b e c , e calcule as raízes reais de um 
-- polinômio do segundo grau

equation a b c
    | a == 0 = error "It's not a quadratic equation"
    | delta < 0 = error "Quadratic equation has no real roots"
    | delta >= 0 = (x1, x2)
    where delta = b*b - 4*a*c
          x1 = (-b + sqrt delta) / (2*a)
          x2 = (-b - sqrt delta) / (2*a)

{- LET <BINDINGS> IN <EXPRESSION>  -}

-- Refaça a função equation usando a 
-- estrutura let in

equation' a b c = 
    let delta = b*b-4*a*c
        x1 = (-b+sqrt delta) / (2*a)
        x2 = (-b-sqrt delta) / (2*a)
    in if (a == 0 || delta < 0)
        then error "error"
        else (x1, x2)