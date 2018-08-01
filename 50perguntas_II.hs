module P_II where
import Data.List

data Movimento = Norte| Sul| Este| Oeste deriving Show
data Posicao = Pos Int Int deriving Show
data Semaforo = Verde| Vermelho| Amarelo deriving Show

-- | 1)  
myEnumFromTo :: Int -> Int -> [Int]
myEnumFromTo x y = if  x <= y 
                   then x : myEnumFromTo (x+1) (y)
                   else []
-- | 2)
myEnumFromThenTo :: Int -> Int -> Int -> [Int]
myEnumFromThenTo x y z = if (y>x) && (z >= y)
                         then x : myEnumFromThenTo y (x-y) z
                         else []
-- | 3)
(+++) :: [a] -> [a] -> [a]
(+++) [] l = l
(+++) l [] = l
(+++) (h:t) l = h : (+++) t l

-- | 4)
(!!!) :: [a] -> Int -> [a]
(!!!) (h:t) 1 = [h]
(!!!) (h:t) x | x <= length (h:t) = (!!!) (t) (x-1)
              | otherwise = []

-- | 5)
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (h:t) = myReverse (t) ++ [h]

-- | 6) 
myTake :: Int -> [a] -> [a]
myTake x [] = []
myTake 1 (h:t) = [h]
myTake x (h:t) | x <= length (h:t) = h : myTake (x-1) t
               | otherwise = h:t
-- | 7):
myDrop :: Int -> [a] -> [a]
myDrop x [] = []
myDrop 0 (h:t)= (h:t)
myDrop 1 (h:t) = t
myDrop x (h:t) | x <= length (h:t) = myDrop (x-1) t 
               | otherwise = []

-- | 8) 
myZip :: [a] -> [b] -> [(a,b)]
myZip [] [] = []
myZip [] _ = []
myZip _ [] = []
myZip (h:t) l = [(h,head l)] ++ (myZip t (tail l))

-- | 9)
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (h:t) = x == h || myElem x t

-- | 10)
myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate x y = y : myReplicate (x-1) y

-- | 11)
myIntersperse :: a -> [a] -> [a]
myIntersperse x [] = []
myIntersperse x [l] = [l]
myIntersperse x (h:t) = (h: x: (myIntersperse x t))

-- | 12)
my_Group :: Eq a => [a] -> [[a]]
my_Group [x] = [[x]]
my_Group [] = []
my_Group l = making_list l : my_Group (myDrop (length (making_list l)) l)

making_list :: Eq a => [a] -> [a]
making_list [x] = [x]
making_list [] = []
making_list (h:m:t) | (h == m) = h : making_list (m:t)
                    | otherwise = [h]


-- | 13)
myConcat ::  [[a]] -> [a]
myConcat [] = []
myConcat l = (head l) ++ myConcat (tail l)

-- | 14)
myInits :: [a] -> [[a]]
myInits [] = [[]]
myInits l = (myInits (menos_ultimo l)) ++ [l]

-- | 15)

myTails :: [a] -> [[a]]
myTails [] = [[]] 
myTails l = [l] ++ (myTails (menos_ultimo l) ++ [])

menos_ultimo :: [a] -> [a]
menos_ultimo [x,y] = [x]
menos_ultimo [x] = []
menos_ultimo (h:t) | length  (h:t) > 1 = h : menos_ultimo t
                   | otherwise = menos_ultimo t
-- | 16)
myIsPrefixOf :: Eq a => [a] -> [a] -> Bool
myIsPrefixOf _ [] = False
myIsPrefixOf [] _ = True
myIsPrefixOf (x:xs) (y:ys) = (x == y) && myIsPrefixOf (xs)(ys)
                    

-- | 17)
myIsSuffixOf :: Eq a => [a] -> [a] -> Bool
myIsSuffixOf _ [] = False
myIsSuffixOf [] _ = True
myIsSuffixOf (x:xs) (y:ys) = myElem x (ys) && myIsSuffixOf (xs)(ys)   

-- 18) 
myIsSubsequenceOf :: Eq a => [a] -> [a] -> Bool
myIsSubsequenceOf [] [] = False
myIsSubsequenceOf _ [] = False
myIsSubsequenceOf [] _ = False
myIsSubsequenceOf (h:t) l | (length (h:t) < length l) && (h == head (tail l)) && ((head t) == (last l)) = True
                          | otherwise = False
-- 19)
myElemIndices :: Eq a => a -> [a] -> [Int]
myElemIndices x [] = []
myElemIndices x (h:t) = count 0 x (h:t)
    
count :: Eq a => Int -> a -> [a] -> [Int]
count c x [] = []
count c x (h:t)
    | x == h = c:count (c+1) x (t)
    | otherwise = count (c+1) x (t)

-- 20)
myNub ::  Eq a => [a] -> [a]
myNub [] = []
myNub (h:t) | myElem h t = myNub t
            | otherwise = h: myNub t

-- 21)
myDelete :: (Eq a) => a -> [a] -> [a]
myDelete _ [] = []
myDelete x (h:t) | x /= h = h:(myDelete x t)
                 | otherwise = (myDelete x t)
-- 22) 
ii :: Eq a => [a] -> [a] -> [a]
ii [] _ = []
ii l [] = l
ii (x:xs) (y:ys) | (x /= y) = x : ii (xs)(ys)
                 | otherwise = ii (xs)(ys) 
-- 23)
myUnion :: Eq a => [a] -> [a] -> [a]
myUnion [] [] = []
myUnion l [] = l
myUnion l (y:ys)| (myElem y (l)) =  myUnion (l)(ys)
                    | otherwise = (myUnion ((+++) l [y]) (ys)) 

myQuickSort :: Ord a => [a] -> [a]
myQuickSort [] = []
myQuickSort (h:t)= (menores t) ++ [h] ++ (maiores t)

menores :: Ord a => [a] -> [a]
menores [] = []
menores (h:t) | (h <= head t) = h : menores t
              | otherwise = menores t

maiores :: Ord a => [a] -> [a]
maiores [] = []
maiores (h:t)| (h > (head t)) = maiores t
             | otherwise = h : maiores t 
-- 24)
myIntersect :: Eq a => [a] -> [a] -> [a]
myIntersect [] _ = []
myIntersect l [] = l
myIntersect (x:xs)(y:ys) | myElem x (y:ys) = x : myIntersect (xs)(y:ys)
                         | otherwise = myIntersect (xs)(y:ys)
-- 25)
myInsert  :: Ord a => a -> [a] -> [a]
myInsert x [] = [x]
myInsert x (h:t) | (x <= h) = x: h: t
                 | otherwise = h: myInsert x t

-- 26)
myUnwords :: [String] -> String
myUnwords [] = ""
myUnwords [x] = x
myUnwords (h:t) = (+++) ((+++) h " ") (myUnwords t) 

-- 27)

myUnlines :: [String] -> String
myUnlines [] = []
myUnlines (h:t) = ((+++) ((+++) h "\n") (myUnlines t))

-- 28)
pMaior :: Ord a => [a] -> Int
pMaior [x] = 0
pMaior (x:y:t) = aux_pMaior 0 (x:y:t)

aux_pMaior :: Ord a => Int-> [a] -> Int
aux_pMaior c [x,y] | x > y = c
                   | otherwise = c+1
aux_pMaior c (x:y:t) | x > y = aux_pMaior (c+1) (x:t)
                     | otherwise = aux_pMaior (c+1) (y:t)


-- 29)
temRepetidos :: Ord a => [a] -> Bool
temRepetidos [] = False
temRepetidos (x:y:t) | myElem x (y:t) || myElem y t = True

-- 30)
algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (h:t) | myElem h ['0'..'9'] = h : algarismos t
                 | otherwise = algarismos t 
-- 31)
posImpares :: [a] -> [a]
posImpares [] = []
posImpares [x] = []
posImpares (h:t) = (head t) : posImpares (tail t)

-- 32)
posPares :: [a] -> [a]
posPares [] = []
posPares [x] = [x]
posPares (h:t) = h: posPares (tail t)

-- 33)
isSorted :: Ord a => [a] -> Bool
isSorted [x,y] | y >= x = True
               | otherwise = False 
isSorted (h:t) = (h <= (head t)) && isSorted (t)

-- 34)
isort :: Ord a => [a] -> [a]
isort [] = []
isort (h:t) = myInsert h (sort t)

-- 35)
menor :: String -> String -> Bool
menor l [] = False
menor l1 l2 | length (l1) < length (l2) = True
            | otherwise = False 

-- 36)
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet a ((x,y):t) = a == x || elemMSet a t

-- 37)
lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((x,y):t)= y + lengthMSet t

-- 38)
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((x,y):t) = myReplicate (y) (x) ++ converteMSet t

-- 39)
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet _ [] = []
insereMSet a ((x,y):t) | (a == x) = (x,y+1) : t
                       | otherwise = (x,y) : insereMSet a t

-- 40)
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = []
removeMSet a  ((x,y):t) | (a == x) = t
                        | otherwise = (x,y) : removeMSet a t

-- 41)
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (x:t) = (x,repetidos(x:t)):constroiMSet (drop (repetidos (x:t)) (x:t))


repetidos :: Ord a => [a] -> Int
repetidos [x] = 1
repetidos (x:y:t) | (x == y) =1+repetidos (y:t)
                  | otherwise = 1

-- 42)

myPartitionEithers :: [Either a b] -> ([a],[b])
myPartitionEithers ([]) = ([],[])
myPartitionEithers (Left a : t)= (a:l,r)
                   where (l,r) = myPartitionEithers t
myPartitionEithers (Right a : t) = (l, a:r)
                   where (l,r) = myPartitionEithers t

-- 43)
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just h :t) = h : catMaybes t
catMaybes (Nothing : t) = catMaybes t

-- 44)
posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) m = (x - mov_Este m  + mov_Oeste m , y - mov_Norte m + mov_Sul m)
                     
                     where mov_Norte [] = 0
                           mov_Norte (Norte:t) = 1 + mov_Norte t
                           mov_Norte (_:t) = mov_Norte t
                           
                           mov_Sul [] = 0
                           mov_Sul (Sul:t) = 1 + mov_Sul t
                           mov_Sul (_ :t) = mov_Sul t
                           
                           mov_Este [] = 0
                           mov_Este (Este :t) = 1 + mov_Este t
                           mov_Este (_ :t) = mov_Este t 
                           
                           mov_Oeste[] = 0
                           mov_Oeste (Oeste :t) = 1 + mov_Oeste t
                           mov_Oeste (_:t) = mov_Oeste t

-- 45)

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (0,0) (0,0) = []
caminho (x,y) (a,b) | (x <= a) && (y > b) = (myReplicate (abs (a-x)) Este) ++ (myReplicate (abs (b-y)) Sul)
                    | (x <= a) && (y <= b) = (myReplicate (abs (a-x)) Este) ++ (myReplicate (abs (b-y)) Norte) 
                    | (x > a) && (y > b) = (myReplicate (abs (a-x)) Oeste) ++ (myReplicate (abs (b-y)) Sul)
                    | (x > a) && (y <= b) = (myReplicate (abs (a-x)) Oeste) ++ (myReplicate (abs (b-y)) Norte)
-- 46)

verticais :: [Movimento] -> Bool
verticais [] = False
verticais (Norte : t) = verticais t
verticais (Sul : t) = verticais t 
verticais (_: t) = False

-- 47)
maiscentral :: [Posicao] -> Posicao
maiscentral [Pos x y] = Pos x y
maiscentral ((Pos x y) : (Pos a b) : t) | (x > a) || ((x == a) && (y >= b)) = maiscentral ((Pos a b) :t)
                                         | otherwise = maiscentral ((Pos x y) : t)  

-- 48)
vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos (Pos x y) [] = []
vizinhos (Pos x y) ((Pos a b) : t) | (x == a)   && (y == b) || 
                                   (x == a - 1) && (y == b) ||
                                   (x == a + 1) && (y == b) ||
                                   (x == a)     && (y == b - 1) ||
                                   (x == a + 1) && (y == b + 1)  = (Pos a b): vizinhos  (Pos x y) t
                                   | otherwise = vizinhos (Pos x y) t     
mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [] = False
mesmaOrdenada ((Pos x y): (Pos a b): t) | (y == b) = mesmaOrdenada ((Pos a b): t)
                                        | otherwise = False

interseccaoOK :: [Semaforo] -> Bool
interseccaoOK s = my_filter s <= 1
               where my_filter (Verde: t) = 1 + my_filter t
                     my_filter (_:t) = my_filter t
                     my_filter [] = 0
