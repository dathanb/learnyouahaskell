maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' (x:[]) = x
maximum' (x:xs) = max x (maximum' xs)

-- Jumping a little bit ahead and trying to use Nothing instead of error for the empty list
--maximum' :: (Ord a) => [a] -> Maybe a
--maximum' [] = Nothing
--maximum' (x:xs) =
--  let maxTail = maximum' xs
--  in case maxTail of
--    Nothing -> Just x
--    Just y -> Just $ if x > y then x else y

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x:(replicate' (n-1) x)

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _ | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:(take' (n-1) xs)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

repeat' :: a -> [a]
repeat' x = x : (repeat' x)

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' needle (hay:haystack)
  | needle == hay = True
  | otherwise = elem needle haystack

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort [a] = [a]
quicksort (x:xs) = 
  let leftSide = quicksort [a | a <- xs, a<x]
      rightSide = quicksort [a | a <- xs, a > x]
  in leftSide ++ [x] ++ rightSide


