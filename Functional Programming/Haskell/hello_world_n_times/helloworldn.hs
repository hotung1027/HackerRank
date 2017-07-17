helloWorld
 n
    |n == 0 = return()
    |n > 0 = helloWorld n-1 -- Complete this function

-- This part is related to the Input/Output and can be used as it is
-- Do not modify it
main = do
    n <- readLn :: IO Int
    helloWorld n
