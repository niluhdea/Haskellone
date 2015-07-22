module One where

x = 10

nama = "eek"
umur = "1 tahun"
alamat = "perut"

add a 0 = a
add a b
  | b > 0 = add (succ a) (pred b)
  | b < 0 = add (pred a) (succ b)

ambil n [] = []
ambil 0 xs = []
ambil n (x:xs) = x:(ambil (n - 1) xs)

cat xs [] = xs
cat [] xs = xs
cat (x:xs) ls = x : (cat xs ls)

buntut (x:xs) = xs

balik [] = []
balik (x:[]) = [x]
balik (x:xs) = cat (balik xs) [x]

head' (x:xs) = x

tail' (x:xs) = xs

null' [] = True
null' _ = False

elem' e [] = False
elem' e (x:xs)
  | e == x = True
  | otherwise = elem' e xs

notElem' e [] = True
notElem' e (x:xs)
  | e == x = False
  | otherwise = notElem' e xs

length' [] = 0
length' (x:xs) = 1 + length' xs

--length [1,2,3] --> length' [2,3]
--length [2,3] --> length' [3]
--length [3] --> 1


sum' [] = 0
sum' (x:xs) = x + sum' xs

product' [] = 1
product' [x] = x
product' (x:xs) = x * product' xs

--product [2,3,4] --> 2 * product' [3,4]
--product [3,4] --> 2 * 3 * product' [4]
--product [4] --> 4

last' [x] = x
last'(x:xs) = last' xs

--last [1,2,3] --> [2,3]

init' [x] = []
init' (x:xs) = x : init' xs

--init [1,2,3] --> [1,2]
--init [1,2] --> [1]
--init [1] --> []

take' 0 _ = []
take' a [] = []
take' a (x:xs) = x : take' (pred a) xs

--take 2 [1,2,3] --> [1,2]

drop' 0 a = a
drop' a [] = []
drop' a (x:xs) = drop' (pred a) xs

--drop 2 [1,2,3] --> [3]

max' a b
  | a >= b = a

min' a b
  | a >= b = b

maximum' [x] = x
maximum' (x:xs) = max' x (maximum' xs)

minimum' [x] = x
minimum' (x:xs) = min' x (minimum' xs)

reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

concat' [] = []
concat' (x:xs) = x ++ concat' xs

fst' (a,b) = a

snd' (a,b) = b

and' [] = True
and' [True, True] = True

and' [True] = True
and' [False] = False
and' [True, True] = True
and' [False, False] = False
and' [True, False] = False
and' [False, True] = False

or' [True] = True
or' [False] = False

zip' [] b = []
zip' [a] [b] = [(a,b)]
zip' [a,b] [c,d] = [(a,c), (b,d)]

or















-- sav
