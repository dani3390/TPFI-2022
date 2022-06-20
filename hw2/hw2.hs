{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


-- data BinTree a = Radice (BinTree a) (BinTree a) | Foglia deriving (Show)

--data BinaryTree a = Leaf
      --            | Nodef (BinaryTree a) a (BinaryTree a)
       --           deriving (Eq, Ord, Show)

data Tree a = Node a (Tree a) (Tree a) | Leaf a deriving Show

data BTree a = Nodea a (BTree a) (BTree a) | EmptyBTa deriving Show


-- ESERCIZIO 1

splitEvery :: [a] -> [[a]]
splitEvery = takeWhile (not.null) . map (take 1) . iterate (drop 1)

mergeSort :: (Ord a) => [a] -> [a]
mergeSort x = mergeSort1 (splitEvery x)

halve :: [[a]] -> ([[a]],[[a]])
halve xs = splitAt half xs
    where half = length xs `div` 2

mergeSort1 :: (Ord a) => [[a]] -> [a]
mergeSort1 [] = []
mergeSort1 [x] = concat [x]
mergeSort1 fs@(x:xs) = merge1 (mergeSort1 l) (mergeSort1 r)
    where (l, r) = halve (splitEvery fs)

merge1 :: (Ord a) => [[a]] -> [[a]] -> [a]
merge1 [] [] = []
merge1 [] ys = concat ys
merge1 xs [] = concat xs
merge1 xs@(x:txs) ys@(y:tys)
    | x < y = x ++ merge1 txs ys
    | otherwise = y ++ merge1 xs tys

-- Il costo totale di questo mergesort dovrebbe essere O(n logn)

-- main :: IO ()
-- main = print(mergeSort [1, 22, 7, 22, 29, 0, 4, 10329, 5, 829])

------------------------------------------------------------------------------------------
data BinTree a = R a (BinTree a) (BinTree a) | F a
               deriving (Show)
-- ESERCIZIO 2


t5 = Node 'a' (Node 'b'( Node 'c' (Leaf 'd') (Leaf 'e')) (Leaf 'f')) (Node 'g' (Leaf 'h') (Leaf 'i'))




--              a
--            /   \
--         b        g
--        / \      / \
--       c   f    h   i
--      / \
--     d   e

-- MAP E FOLD: qui ho 2 diverse implementazioni di fold perché sfortunatamente non sono riuscito in 
-- in tempo a implementare come avrei avoluto una singola.
-- In questo caso il foldrBT visita ottimamente l'albero dando un giusto risultato della visita
-- mentre foldBT, seppur funzionando comunque a modo non dando però i valori delle foglie, mi è più semplice da usare per i calcoli successivi

mapBT :: (a -> b) -> Tree a -> Tree b
mapBT f (Leaf a) = Leaf (f a)
mapBT f (Node x l r) = Node (f x) (mapBT f l) (mapBT f r)

foldBT :: (a -> b -> b -> b) -> b -> Tree a -> b
foldBT f e (Leaf a)     = e
foldBT f e (Node x l r) = f x (foldBT f e l) (foldBT f e r)

foldrBT :: (a -> b -> b) -> b -> Tree a -> b
foldrBT f z (Leaf x) = f x z
foldrBT f z (Node x lt rt) = f x (foldrBT f (foldrBT f z rt) lt)

--main :: IO ()
--main = print(foldrBT (\v l -> l++(v:"")) "" t5)


-- FUNZIONI ESERCIZIO 2

-- calcolo numero di nodi
nodes :: Tree a -> Integer
nodes = foldBT  (\_ l r -> 1 + l + r) 0

-- altezza albero
height :: Tree a -> Integer
height= foldBT  (\_ l r -> 1 + max l r) 1

-- altezze destre e sinistre per sbilanciamento (brutto ma efficace)
leftHeight :: Tree a -> Integer
leftHeight = foldBT (\_ l _ -> 1 + l) 1

rightHeight :: Tree a -> Integer
rightHeight = foldBT (\_ _ r -> 1 + r) 1

-- sbilanciamento albero
sbilanciamento :: (Ord a) => Tree a -> Integer
sbilanciamento (Node x l r) = max left right - min left right
    where
        left = leftHeight l
        right = rightHeight r



-- main :: IO ()
-- main = print(sbilanciamento t5)


-- ESERCIZIO 3

data TreeN a = N a [TreeN a] deriving Show
data Tree1 a = Node1 a [Tree1 a] deriving Show

tree1 = N 2 [N 7 [], N 3 [N 0 []], N 1 [N 3 [], N 2 []]]

tt = N 'a' [N 'b' [], N 'c' [N 'd' [], N 'e' []], N 'f' []]

t = N 1 [N 2 [N 3 []], N 4 [], N 5 [N 6 []]]

ttt = Node1 1 [Node1 2 [Node1 4 [], Node1 5 []],
            Node1 3 [Node1 6 []]]

-- foldrT :: ([b] -> a -> b) -> TreeN a -> b
-- foldrT f (N a bs) = f (map (foldrT f) bs) a


mapT :: (a -> b) -> TreeN a -> TreeN b
mapT f (N x xs) = N (f x) (map (mapT f) xs)

foldrT :: (a -> [b] -> b) -> b -> TreeN a -> b
foldrT f z (N x xs) = f x $ map (foldrT f z) xs



-- ESERCIZIO 5

{--

Le definizioni delle varie funzioni le ho prese da qui: https://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.List.html

CASO BASE

scanr f e [] = [e]

CASO INDUTTIVO

scanr f e (x:xs)
	= {definizione di scanr e .}
map (foldr f e) (tails (x:xs))
	= {definizione di tails}
map (foldr f e) ((x:xs) : tails xs)
	= {definizione di map}
foldr f e (x:xs) : map (foldr f e) (tails xs)
	= {definizione di foldr}
f x (foldr f e xs) : scanr f e xs
	= {dirò che foldr f e xs = head (scanr f e xs)}
f x (head ys) : ys
	where ys = scanr f e xs

------------------

DIMOSTRAZIONE: foldr f e xs = head (scanr f e xs)

head (scanr f e xs)
	= {definizione di scanr e .}
head (map (foldr f e) (tails (xs)))
	= {definizione di tails}
head (map (foldr f e) ((_:xs) : tails xs)))
	= {definizione di map}
head (foldr f e (_:xs) : map (foldr f e) (tails xs)))
	= {definizione di head}
foldr f e (_:xs)
	= {definizione di foldr}
foldr f e xs

--}