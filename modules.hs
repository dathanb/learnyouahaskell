import Data.List (nub, sort)
import qualified Data.Map as Map

intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ ( x:[] ) = [x]
intersperse' e ( x:xs ) = x : e : intersperse' e xs

flatten :: (Foldable t) => t [a] -> [a]
flatten = foldr1 (\x acc -> x ++ acc)

intercalculate' :: [a] -> [[a]] -> [a]
intercalculate' i xs = foldr1 (\x acc -> x ++ i ++ acc) xs

transpose' :: [[a]] -> [[a]]
transpose' [] = []
transpose' ([] : xss) = transpose' xss
transpose' ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : transpose' (xs : [t | _:t <- xss])

and' :: (Foldable t) => t Bool -> Bool
and' = foldl1 (\acc x -> x && acc)

or' :: (Foldable t) => t Bool -> Bool
or' = foldl1 (\acc x -> x || acc)

-- If we limit ourselves to [a] instead of Foldable of a, then these work
-- But since map requires a list, we can't use these constructions on arbitrary foldables.
--all' :: (a -> Bool) -> [a] -> Bool
--all' f = and' . map f
--
--any' :: (a -> Bool) -> [a] -> Bool
--any' f = or' . map f

-- instead, for arbitrary foldable structures
all' :: (Foldable t) => (a -> Bool) -> t a -> Bool
all' f = foldr (\x acc -> f x && acc) True

any' :: (Foldable t) => (a -> Bool) -> t a -> Bool
any' f = foldr (\x acc -> f x || acc) True

iterate' :: (a -> a) -> a -> [a]
iterate' f x = next : iterate' f next
  where next = f x

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f ( x:xs )
  | pred = x : takeWhile' f xs
  | otherwise = []
  where pred = f x

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (x:xs)
  | pred = dropWhile' f xs
  | otherwise = x:xs
  where pred = f x

span' :: (a -> Bool) -> [a] -> ([a], [a])
span' _ [] = ([], [])
span' f xs = span'' f ([], xs)
  where span'' _ (left, []) = (left, [])
        span'' f (left, right@(r:rs))
          | pred = span'' f (left ++ [r], rs)
          | otherwise = (left, right)
          where pred = f r

break' :: (a -> Bool) -> [a] -> ([a], [a])
break' f = span' $ not . f

group' :: (Eq a) => [a] -> [[a]]
group' = foldr group'' []
  where group'' x [] = [[x]]
        group'' x (acc@(r:rs)) = if x == head r then ((x:r):rs) else [x]:acc

init' :: [a] -> [a]
init' (x:[]) = []
init' (x:xs) = x : init' xs

inits' :: [a] -> [[a]]
inits' = reverse . scanr (\x acc -> x:acc) []

tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' xs = xs : tails' (tail xs)

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
  let len = length needle
   in foldl (\acc x -> if take len x == needle then True else acc) False (tails' haystack)

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' needle (x:xs) = if needle == x then True else elem' needle xs
-- I originally implemented this using foldl like #search, but using the lazy version above is significantly faster
-- than an eager #foldl for large lists
-- elem' needle haystack = foldl (\acc x -> if x == needle then True else acc) False haystack

partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' f = foldr (\x (left, right) -> if f x then (x:left, right) else (left, x:right)) ([], [])

find' :: (a -> Bool) -> [a] -> Maybe a
find' _ [] = Nothing
find' p (x:xs) = if p x then Just x else find' p xs

elemIndex' :: (Eq a) => a -> [a] -> Maybe Int
elemIndex' _ [] = Nothing
elemIndex' needle (x:xs)
  | needle == x = Just 0
  | otherwise = fmap (+1) $ elemIndex' needle xs

elemIndices' :: (Eq a) => a -> [a] -> [Int]
elemIndices' _ [] = []
elemIndices' needle haystack = elemIndices'' needle (zip [0..] haystack)
  where elemIndices'' _ [] = []
        elemIndices'' needle ((index, element):xs)
          | needle == element = index : elemIndices'' needle xs
          | otherwise = elemIndices'' needle xs

lines' :: String -> [String]
lines' input = case span (/= '\n') input of
                 ([], []) -> []
                 (x, []) -> [x]
                 (x, y) -> x:(lines' . tail $ y)

unlines' :: [String] -> String
unlines' ls = foldr1 (\x acc -> x ++ '\n' : acc) ls

-- maps
--findKey :: (Eq k) => k -> [(k,v)] -> v
--findKey key xs = snd . head . filter (\(k, v) -> key == k) $ xs
-- safer version
--findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
--findKey _ [] = Nothing
--findKey key ((k,v):xs) = if key == k
--                            then Just v
--                            else findKey key xs

fromList' :: (Ord k) => [(k,v)] -> Map.Map k v
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty

