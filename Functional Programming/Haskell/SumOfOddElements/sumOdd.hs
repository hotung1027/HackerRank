packageSum :: [Int] -> Int
packageSum (x:xs)  = x + packageSum xs
package _     = 0
