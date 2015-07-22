import Data.List

and' [] = True
and' (x:xs) = x && and' xs
