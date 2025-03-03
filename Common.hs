import Data.List

-------------------Question 2.1-------------------
digits :: Int -> [Int]
digits n = map (read . (:[])) (show n)

-------------------Question 2.2-------------------
generator2 :: [(Int, Int, Int, Int, Int, Int, Int, Int)]
generator2 = [ (a1, a2, a3, a4, a5, a6, a7, a8) 
            | a4 <- validNumbers 
            , a1 <- validNumbers, a1 > a4, notElem a1 [a4]
            , a2 <- validNumbers, a2 > a4, notElem a2 [a4, a1]
            , a3 <- validNumbers, a3 > a4, notElem a3 [a4, a1, a2]
            , a5 <- validNumbers, a5 > a4, notElem a5 [a4, a1, a2, a3]
            , a6 <- validNumbers, a6 > a4, notElem a6 [a4, a1, a2, a3, a5]
            , a7 <- validNumbers, a7 > a4, notElem a7 [a4, a1, a2, a3, a5, a6]
            , a8 <- validNumbers, a8 > a4, notElem a8 [a4, a1, a2, a3, a5, a6, a7]
            ]

validNumbers :: [Int]
validNumbers = [n | n <- [10..31], isValidSquare n]

isValidSquare :: Int -> Bool
isValidSquare n = 
  let sq = n * n
      digits = show sq
  in length digits == 3 && length (nub digits) == 3
-------------------Question 2.3-------------------
selector2 :: (Int, Int, Int, Int, Int, Int, Int, Int) -> Bool
selector2 (a1, a2, a3, a4, a5, a6, a7, a8) =
    let
        ages = [a1, a2, a3, a4, a5, a6, a7, a8]
        pairs = [(i, j) | i <- [0..7], j <- [i+1..7]]
    in
        all (\(i, j) -> digitOverlap (ages !! i) (ages !! j) == 
                        letterOverlap (names !! i) (names !! j)) pairs

digitOverlap :: Int -> Int -> Bool
digitOverlap x y = not . null $ nub (show (x * x)) `intersect` nub (show (y * y))

letterOverlap :: String -> String -> Bool
letterOverlap name1 name2 = not . null $ nub name1 `intersect` nub name2

names :: [String]
names = ["alan", "cary", "james", "lucy", "nick", "ricky", "steve", "victor"]

main :: IO ()
main = 
    print (filter selector2 generator2)
