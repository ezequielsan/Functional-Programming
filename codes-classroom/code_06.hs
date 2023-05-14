data List a = Empty | Node a (List a)
                deriving (Show)


-- Lista encadeada com 3 elementos: [8, 7, 5]
list1 = Node 8 (Node 7 (Node 5 Empty))

add x Empty = Node x (Empty)
add x ls = Node x ls 