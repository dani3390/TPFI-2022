
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use camelCase" #-}

-- ESERCIZIO 1


insonnia :: String
insonnia = concatMap pecora numero where
    numero = 1:map (+1) numero
    pecora x = show x++ " sheep "


-- main = print(insonnia)

-- ESERCIZIO 2


infinite :: [Integer] -> [Integer]
infinite riga = zipWith (+) (0:riga) (riga++[0])

tartaglia :: [[Integer]]
tartaglia = iterate infinite [1]

-- con l'iterate sulla funzione che crea le righe vado a creare la lista infinita
-- con lo zipwith invece vado ogni volta a crearmi la nuova riga in base alla precedente (per via dell'iterate) mantenendo ai lati ovviamente gli 1
-- se iniziassi con iterate infinite [0] avrei tutti 0 

-- main = print(take 20 tartaglia)

-- ESERCIZIO 3

lucky = sieve 3 [1, 3..]
    where
      sieve k (s:xs:xss) =
        s:sieve (k + 1) (xs:fs)
          where 
            fs = [num | (div, num) <- zip [k..] xss, mod div xs /= 0]



-- main = print(take 40 lucky)



-- ESERCIZIO 4

{-
allSums :: [Integer]->[[Integer]]
allSums (x:xs) = map (x+) xs : allSums xs
allSums [] = []

nextUlams n us rs
  | isUlam n 0 us rs = n: nextUlams (n+1) us (n:rs)
  | otherwise = nextUlams (n+1) us rs
  where
  isUlam n 2 _ _ = False
  isUlam n k us@(u:tus) rs@(r:trs)
    | r<=u = k == 1
    | s==n = isUlam n (k+1) tus trs
    | s <n = isUlam n k tus rs
    | otherwise = isUlam n k us trs
    where s = u + r


-- provol 

func :: [[Integer]]->[Integer]
func [[x]] = nextUlams 3 ulams2 [x]
func [[]] = []
func _ = []


ulams2 = 1:2:func (allSums ulams2)
ulams = 1:2:nextUlams 3 ulams [2,1]



allSums2 :: Num a => [a]->[[a]]
allSums2 (x:xs) = map (x+) xs:allSums2 xs
allSums2 [] = [[]]

crop :: [[a]]->[[a]]->[[a]]
crop xss (ys:yss) = map head xss:crop (ys:map tail xss) yss


diags :: [[a]] -> [[a]]
diags (xs:xss) = crop [xs] xss

ulams22 = diags (allSums2 ulams22)



-- main = print(allSums [4, 5, 6, 7, 8, 8, 8, 8, 9, 1, 2, 3])
main = print(diags (allSums2 [2, 3, 4, 5, 6, 7]))
-}