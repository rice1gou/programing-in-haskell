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
