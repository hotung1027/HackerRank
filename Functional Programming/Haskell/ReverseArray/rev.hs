rev :: [Int]->[Int]
rev (x:xs) = rev xs++[x]
rev _=     []



main :: IO ()
main = print (rev [50..100])
