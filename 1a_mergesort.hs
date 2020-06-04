{-
merge:: [String] -> [String] -> [String]
merge a b
 | null a = b
 | null b = a
 | head a <= head b = head a : merge (tail a) b
 | otherwise = head b : merge a (tail b)
-}

merge:: [String] -> [String] -> [String]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
 | x <= y = x:(merge xs (y:ys))
 | otherwise = y:(merge (x:xs) ys)

--main = print (merge["a","c","k"] ["b","d","f"])

{-lefthalf :: [String] -> [String]
lefthalf a = take (div(length a) 2) a-}

lefthalf :: [String] -> [String]
lefthalf a = take (length a `div` 2) a
--main = print (lefthalf["a","b","c","d"])

righthalf :: [String] -> [String]
righthalf a = drop (length a `div` 2) a

mergesort :: [String] -> [String]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort (lefthalf xs)) (mergesort (righthalf xs))

main = print (mergesort ["a","c","k","b"])
