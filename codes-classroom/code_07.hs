import Data.Char (toUpper) 
-- main = putStrLn "Hello, Worl

isVogal :: Char -> Bool
isVogal c = c `elem` "aeiouAEIOU"

onlyChar :: [Char] -> [Char]
onlyChar str = [c | c <- str, isVogal c]

main = do
    putStr "Digite uma String: "
    x <- getLine 
    putStr "A string em maisuculas é: "
    let y = onlyChar [ toUpper c | c <- x ]
    putStrLn y
