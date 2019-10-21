-- Exercise 1
last' :: [a] -> a
last' xs = head(reverse(xs))

last'' :: [a] -> a
last'' xs = xs !! (length(xs) - 1)

-- Exercise 2
third :: [a] -> a
third xs   |  length(xs) >= 3 = head(tail(tail(xs)))
           |  otherwise       = error "Does not have three elements!"

third' :: [a] -> a
third' xs  |  length(xs) >= 3 = xs !! 2
           |  otherwise      = error "Does not have three elements!"

third'' :: [a] -> a
third'' [] = error "Does not have three elements!"
third'' (_:[]) = error "Does not have three elements!"
third'' (_:_:[]) = error "Does not have three elements!"
third'' (_:_:x:_) = x

-- Exercise 3
safetail :: [a] -> [a]
safetail xs = if null(xs) then [] else tail(xs)

safetail' :: [a] -> [a]
safetail' xs | null(xs) = []
             | otherwise = tail(xs)

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (x:xs) = xs

-- Pattern matching produced the most concise code.

-- Exercise 4
halve :: [a] -> ([a],[a])
halve xs | length(xs) `mod` 2 == 0 = (take (length(xs) `div` 2) xs , drop (length(xs) `div` 2) xs)
         | otherwise = error "List isn't of even length!"

-- Exercise 5
enc :: Int -> String -> String
enc _ [] = [] 
enc n (x:xs) = [toEnum(fromEnum x + n)] ++ enc n xs

encrypt :: Int -> String -> (String, String -> String)
encrypt n xs = (enc n xs, enc (-n))

-- Exercise 6
luhnDouble :: Int -> Int
luhnDouble n | ((n * 2) >= 9) = (n * 2) - 9
             | otherwise = (n * 2)

luhn :: Int -> Int -> Int -> Int -> Bool
luhn n x y z | (luhnDouble(n) + x + luhnDouble(y) + z) `mod` 10 == 0 = True
             | otherwise = False