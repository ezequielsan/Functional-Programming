import System.IO

bkLine ln = (head w, drop 1 w)
    where w = words ln

studentName mat cont = all !! 0
    where ls = lines cont
          a = [ bkLine ln | ln <- ls]
          all = [ n | (m,n) <- a, m == mat ]
main = do
    h <- openFile "students.txt" ReadMode
    contents <- hGetContents h
    putStrLn (studentName "33333" contents)
    hClose h