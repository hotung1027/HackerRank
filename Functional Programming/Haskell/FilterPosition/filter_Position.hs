f :: [Int] -> [Int]
f (_:x:arr) =x:f arr
f _=        []

-- Fill up this Function

-- This part deals with the Input and Output and can be used as it is. Do not modify it.
main::IO()
main = do
   inputdata <- getContents
   mapM_ print. f. map read. lines $ inputdata
