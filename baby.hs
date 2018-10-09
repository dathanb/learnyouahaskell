doubleMe :: Int -> Int
doubleMe x = x + x

doubleUs :: Int -> Int -> Int
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber :: Int -> Int
doubleSmallNumber x = if x < 100
                         then x * 2
                         else x

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase ls = [x | x <- ls, x `elem` ['A'..'Z']]

texas :: Int -> Int -> [Int]
texas c n = if c == n
                then [n]
                else c : (texas (c+1) n)

length' :: [t] -> Int
length' xs = sum [1 | _ <- xs]

zip' :: [t1] -> [t2] -> [(t1, t2)]
zip' x y = if null x || null y
              then []
              else (head x, head y) : zip' (tail x) (tail y)

addThree :: Int -> Int -> Int -> Int
addThree x y z = sum [x,y,z]
