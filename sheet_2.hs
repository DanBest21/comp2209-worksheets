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
