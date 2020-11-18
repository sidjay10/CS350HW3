-- Siddharth Jayashankar (170699)
-- Question 1
-----------------------------------------------------------------------------
sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = 
    (sort $ filter (\y -> y <= x) xs ) ++ 
        [x] ++ (sort $ filter (\y -> y > x) xs )
-----------------------------------------------------------------------------

-- Question 2
-----------------------------------------------------------------------------
uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) =  [x] ++ (uniq $ filter (\y -> y /= x) xs)
-----------------------------------------------------------------------------

-- Question 3
-----------------------------------------------------------------------------
neighbors :: (Ord a1, Ord a2, Num a1, Num a2) => a1->a2->[(a1,a2)]
neighbors x y = [(a,b) | a <- [x-1,x,x+1],
                         b <- [y-1,y,y+1],
                         a >= 0, a<= 9,
                         b >= 0, b<= 9,
                         (a,b) /= (x,y)]
-----------------------------------------------------------------------------

-- Question 4
-----------------------------------------------------------------------------
words_in_str :: [Char] -> Int
words_in_str a = foldl (+) 0 $ map (\x->1) $ words a
-----------------------------------------------------------------------------


-- Question 5
-----------------------------------------------------------------------------
compose_multiple :: [b->b] -> b -> b
compose_multiple b a = foldr (\x y -> (x y)) a b
-----------------------------------------------------------------------------

-- Question 6
-----------------------------------------------------------------------------
data BinaryTree a = Nil | Node a (BinaryTree a) (BinaryTree a) deriving (Show,Eq)

maptree :: (a->b) -> BinaryTree a -> BinaryTree b
maptree _ Nil = Nil
maptree f (Node a l r) = Node (f a) (maptree f l)  (maptree f r)

foldTree :: (a->b->b->b) -> b -> BinaryTree a -> b
foldTree _ b Nil = b
foldTree f b (Node a l r) = f a (foldTree f b l) (foldTree f b r)
-----------------------------------------------------------------------------
