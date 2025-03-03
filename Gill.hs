import Data.List


-------------------Question 1.1-------------------
number :: [Int] -> Int
number = foldl (\acc x -> acc * 10 + x) 0

-------------------Question 1.2-------------------
generator1 :: [([Int], [Int], [Int])]
generator1 = [ ([a, b, c], [d, e, f], [g, h, i]) |
                           [a, b, c, d, e, f, g, h, i, j] <- permutations [0..9] ]

-------------------Question 1.3-------------------                           
selector1 :: ([Int],[Int],[Int]) -> Bool
selector1 (as,bs,cs)
    = (isPrime a) && (isPrime b) && (isPrime c) && (sumIsOdd a) && (sumIsOdd b) && (sumIsOdd c) && equalSpace a b c
    where
    a = number as
    b = number bs
    c = number cs
    
isPrime :: Int -> Bool
isPrime n
  | n < 2     = False
  | otherwise = null [x | x <- [2..floor . sqrt $ fromIntegral n], n `mod` x == 0]
  
sumIsOdd :: Int -> Bool
sumIsOdd 0 = False 
sumIsOdd n = odd (sum (digits n))
    where
        digits 0 = []
        digits x = digits (x `div` 10) ++ [x `mod` 10]
    
equalSpace :: Int -> Int -> Int -> Bool
equalSpace a b c = 
    let [x,y,z] = [a, b, c]
    in (y-x) == (z-y)

main :: IO ()
main = 
    print (filter selector1 generator1)
