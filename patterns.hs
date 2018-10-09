lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky n = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

factorial :: (Integral a) => a -> a
factorial 1 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Alfa"
charName 'b' = "Bravo"
charName 'c' = "Charlie"
charName 'd' = "Delta"
charName 'e' = "Echo"
charName 'f' = "Foxtrot"
charName 'g' = "Golf"
charName 'h' = "Hotel"
charName 'i' = "India"
charName 'j' = "Juliet"
charName 'k' = "Kilo"
charName 'l' = "Lima"
charName 'm' = "Mike"
charName 'n' = "November"
charName 'o' = "Oscar"
charName 'p' = "Papa"
charName 'q' = "Quebec"
charName 'r' = "Romeo"
charName 's' = "Sierra"
charName 't' = "Tango"
charName 'u' = "Uniform"
charName 'v' = "Victor"
charName 'w' = "Whiskey"
charName 'x' = "X-Ray"
charName 'y' = "Yankee"
charName 'z' = "Zulu"
charName c = "Unknown"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
-- addVectors a b = (fst a + fst b, snd a + snd b)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (a, _, _) = a

second :: (a, b, c) -> b
second (_, b, _) = b

third :: (a, b, c) -> c
third (_, _, c) = c

head' :: [a] -> a
head' [] = error "can't call head on empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (a:[]) = "The list has one element, and it is " ++ show a
tell (a:b:[]) = "The list has two elements, and they are " ++ show a ++ " and " ++ show b
tell (a:b:_) = "The list is loong. The first two elements are " ++ show a ++ " and " ++ show b

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

firstLetter :: String -> String
firstLetter [] = "Oops! Empty string"
firstLetter all@(x:_) = "The first letter of " ++ all ++ " is " ++ [x]

--bmiTell :: (RealFloat a) => a -> String
--bmiTell bmi
--  | bmi <= 18.5 = "Underweight"
--  | bmi <= 25.0 = "Normal"
--  | bmi <= 30.0 = "Overweight"
--  | otherwise = "Obese"
--
--bmiTell' :: (RealFloat a) => a -> a -> String
--bmiTell' weight height
--  | weight / height ^ 2 <= 18.5 = "Underweight"
--  | weight / height ^ 2 <= 25.0 = "Normal"
--  | weight / height ^ 2 <= 30.0 = "Overweight"
--  | otherwise = "Obese"

max' :: (Ord a) => a -> a -> a
max' a b | a > b = a | otherwise = b

compare' :: (Ord a) => a -> a -> Ordering
compare' a b | a > b = GT | a == b = EQ | otherwise = LT

--bimTell :: (RealFloat a) => a -> a -> String
--bmiTell weight height
--  | bmi <= 18.5 = "Underweight"
--  | bmi <= 25.0 = "Normal"
--  | bmi <= 30.0 = "Overweight"
--  | otherwise = "Obese"
--  where bmi = weight / height ^ 2

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "Underweight"
  | bmi <= normal = "Normal"
  | bmi <= heavy = "Overweight"
  | otherwise = "Obese"
  where bmi = weight / height ^ 2
        (skinny, normal, heavy) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w,h) <- xs]
  where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + topArea * 2

