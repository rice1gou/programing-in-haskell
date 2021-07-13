-- problem1
halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
  where
    n = length xs `div` 2

-- problem2
third1 :: [a] -> a
third1 xs = head (tail (tail xs))

third2 :: [a] -> a
third2 xs = xs !! 2

third3 :: [a] -> a
third3 (_ : _ : x : _) = x

--problem3
safetail1 :: [a] -> [a]
safetail1 xs = if null xs then [] else tail xs

safetail2 :: [a] -> [a]
safetail2 xs
  | null xs = []
  | otherwise = tail xs

safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 (_ : xs) = xs

--problem4

(||) :: Bool -> Bool -> Bool
True || True = True
True || False = True
False || True = True
False || False = False

--problem5

(&&) :: Bool -> Bool -> Bool
x && y =
  if x
    then if y then True else False
    else False

--problem7

multi :: Int -> Int -> Int -> Int
multi = \x -> (\y -> (\z -> x * y * z))

--problem8

luhnDouble :: Int -> Int
luhnDouble n
  | n > 9 = n -9
  | otherwise = n

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d
  | x `mod` 10 == 0 = True
  | otherwise = False
  where
    x = sum [a, luhnDouble b, c, luhnDouble d]
