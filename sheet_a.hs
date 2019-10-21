-- Exercise A1
histogram :: Int -> [Int] -> [Int]
histogram n xs | xs == []  = []
               | otherwise = [ x | x <- xs, n-n >= x >= n ]