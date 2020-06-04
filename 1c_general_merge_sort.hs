-- Exercise 31

lefthalf :: [String] -> [String]
lefthalf a = take (length a `div` 2) a

righthalf :: [String] -> [String]
righthalf a = drop (length a `div` 2) a

merge:: [String] -> [String] -> (String -> String -> Bool) -> [String]
merge xs [] p = xs
merge [] ys p = ys
merge (x:xs) (y:ys) p
 | p x y = x:(merge xs (y:ys) p)
 | otherwise = y:(merge (x:xs) ys p)

mergesort :: [String] -> (String -> String -> Bool) -> [String]
mergesort [] p = []
mergesort [x] p = [x]
mergesort xs p = merge (mergesort (lefthalf xs) p) (mergesort (righthalf xs) p) p

main = print (mergesort ["Thomasina","Richa","Harriet","Thomas","Richard","Harry"] (<))
