square:: [Float] -> [Float]

square [] = []
square [x] = [x*x]

square (x:xs) = x*x : square xs

double:: Float -> Float
double x = 2*x

apply:: (Float -> Float) -> [Float] -> [Float]
apply f []= []
apply f [x] = [f x]
apply f (x:xs) = f x : apply f xs
