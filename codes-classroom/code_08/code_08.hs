import System.IO

bkLine ln = (head w, drop 1 w) 
    where w = words ln

getName mat ls = [ concat (snd x) | x <- ls, fst x == mat]

studentName mat cont = all !! 0
    where ls = lines cont
          a = [ bkLine ln | ln <- ls]
          all = [ n | (m,n) <- a, m == mat ]
main = do
    h <- openFile "students.txt" ReadMode
    contents <- hGetContents h
    let xLines = lines contents
        std = [ bkLine x | x <- xLines ]
    putStrLn (show std)
    hClose h
