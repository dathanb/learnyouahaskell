multThree :: (Num a) => a -> a -> a -> a
multThree a = (\ b -> (\ c -> a * b * c ) )

compareWithHundred :: (Num a, Ord a) => a -> Ordering
--compareWithHundred x = compare 100 x
compareWithHundred = compare 100

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys

zip' :: [a] -> [b] -> [(a,b)]
zip' = zipWith' (,)

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

-- commented out here because it's reimplemented below
--map' :: (a -> b) -> [a] -> [b]
--map' _ [] = []
--map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | predicate == True = x : filter' f xs
  | otherwise = filter' f xs
  where predicate = f x

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let left = quicksort (filter (<=x) xs)
      right = quicksort (filter (>x) xs)
  in left ++ [x] ++ right

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
  where p x = x `mod` 3829 == 0

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
  | predicate == True = x : takeWhile' f xs
  | otherwise = []
  where predicate = f x

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
  | even n = n : collatz (n `div` 2)
  | odd n = n : collatz (n*3 + 1)

countLongChains :: (Integral a) => [a] -> Int
countLongChains xs = length (filter (>15) (map length (map collatz xs)))

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> x + acc) 0 xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' needle haystack = foldl (\acc x -> acc || x == needle) False haystack

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

maximum_fold :: (Ord a) => [a] -> a
maximum_fold xs = foldl1 (\acc x -> if x > acc then x else acc) xs

reverse_fold :: [a] -> [a]
reverse_fold xs = foldl (\acc x -> x : acc) [] xs

product_fold :: (Num a) => [a] -> a
product_fold = foldr1 (*)

filter_fold :: (a -> Bool) -> [a] -> [a]
filter_fold predicate xs = foldr (\x acc -> if predicate x then x:acc else acc) [] xs

head_fold :: [a] -> a
head_fold = foldr1 (\x _ -> x)

last_fold :: [a] -> a
last_fold = foldl1 (\x _ -> x)

cumSqrtSums :: Int
--cumSqrtSums = length (takeWhile (<=1000) (scanl (+) 0 (map sqrt [1..])))
--rewritten with $
cumSqrtSums = length $ takeWhile (<=1000) $ scanl (+) 0 $ map sqrt [1..]


