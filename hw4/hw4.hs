{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- ESERCIZIO 1

numero :: Int -> IO Int
numero n = do {
    if n==0 then
        return 0;
    else do {
        x <- getLine;
        y <- numero (n-1);
        return ((read x :: Int) + y);
    }
}

adder :: IO ()
adder = do {
    putStrLn "Inserisci la quantità di numeri che vuoi inserire: ";
    nums <- numero 1;
    putStrLn ("Digita gli " ++ show nums ++ " numeri scelti.");
    sum <- numero nums;
    putStrLn ("La somma dei numeri inseriti è " ++ show sum);
}

-- ESERCIZIO 2

subsets :: [a] -> [[a]]
subsets (x:xs) =  ((pure (x:) <*> subsets xs) ++ subsets xs)
subsets [] = [[]]

divisori :: Int -> [Int]
divisori x = [y | y <- [1..x-1], x `mod` y == 0]

main = print(filter (\x -> sum x == 24) (subsets(divisori(24))))

-- ESERCIZIO 3.1

data Either' a b = Left1 a | Right1 b


instance Functor (Either' e) where
    fmap _ (Left1 x)  = Left1 x
    fmap f (Right1 y) = Right1 (f y)
-- sotto

instance Applicative (Either' e) where
    pure             =  Right1
    Left1  e  <*>  _  =  Left1 e
    Right1 f  <*>  r  =  fmap f r


instance Monad (Either' e) where
    Left1  l >>= _ = Left1 l
    Right1 r >>= f = f r