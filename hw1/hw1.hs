import Data.List (tails, sortOn, inits)
import Distribution.Simple.Setup (trueArg)

-- ESERCIZIO 1

-- 1.1 e 1.3 (dopo vari test ho notato che la prima implementazione funziona in entrambi i casi)
shuffle :: Eq a => [a] -> [a] -> [a] -> Bool

shuffle (x:xs) (y:ys) (z:zs) | x == z = shuffle xs (y:ys) zs
    | y == z = shuffle (x:xs) ys zs
    | otherwise = False

shuffle [] (y:ys) (z:zs) | y == z = shuffle [] ys zs
    | otherwise = False

shuffle (x:xs) [] (z:zs) | x == z = shuffle xs [] zs
    | otherwise = False

shuffle [] [] [] = True
shuffle [] [] _ = False
shuffle [] _ [] = False
shuffle _ [] [] = False
shuffle _ _ [] = False

-- 1.2

-- Io avrò xs e ys, le due liste su cui dovrò fare dei controlli per verificare che zs sia effettivamente uno shuffle.
-- L'esercizio richiede che io usi una funzione immaginaria genshuffle che va a creare tutti quanti gli shuffle di xs e ys,
-- avendo così una gigantesca lista di liste (che diventa tanto più grande quanti sono gli elementi totali di xs e ys) con
-- tutti quanti i possibili shuffle tra quelle due, chiamiamo in modo immaginario questa lista 'shuffleList'.
-- Già fare questo è assai inefficiente, dal momento che un normalissimo controllo con uno shuffle normale sarebbe sufficiente.
-- Ora mi ritrovo a decidere come andare effettivamente a controllare se le due liste sono uguali.
-- Attualmente l'unica cosa che potrei fare è andare a scorrere man mano shuffleList e controllare che la lista che sto controllando
-- in quel momento sia uguale a zs, ma questo sarebbe molto inefficiente, dal momento che per il solo scorrimento sarei in un tempo O(n). 

-- ESERCIZIO 2

segments :: [a] -> [[a]]
segments = ([]:) . filter (not . null) . concatMap inits . tails

-- per prima cosa sono sicuro di aggiungere sempre la lista vuota
-- poi vado a fare una map che concatena gli inits, ovvero tutte le sequenze crescenti della lista,
-- a tails, ovvero tutte quante le sequenze decrescenti della lista, 
-- infine dal momento che tali inits e tails andrebbero a tirarmi fuori anche diverse liste vuote
-- faccio un filter che va ad eliminarle tutte

-- ESERCIZIO 3

-- 3.1
applyL :: [a -> b] -> [a] -> [b]
applyL (f:fs)(x:xs)= f x : applyL fs xs
applyL _ _ = []

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f xs = applyL (map f xs)

-- 3.2
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f v (x:xs) = f x (foldr f v xs)
myFoldr f v [] = v

myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldr (\y ys -> f y:ys) []

-- 3.3
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f v (x:xs) = foldl f (f v x) xs
myFoldl f v [] = v

myMap2 :: (a -> b) -> [a] -> [b]
myMap2 f = myFoldl (\ys y -> ys++[f y]) []

-- 3.4

-- Perché non è possibile definire i due fold con map?
-- Quando ci si pone una tale domanda è bene andare a vedere la definizione delle funzioni in questione.
-- Foldl e foldr prendono in input una funzione, un argomento e una lista, ed entrambi vanno poi ad applicare
-- a tale lista la funzione con quell'argomento, solo che nel caso di foldr si parte dall'ultimo elemento 
-- mentre in foldl dal primo (e questo porta ovviamente alla sostanziale differenza tra i due).
-- Ma qual è il loro output? Un valore.
-- Vediamo ora map: prende in input una funzione e una lista, su cui applicherà tale funzione (ovviamente si intendono i singoli valori).
-- L'output questa volta è una lista.
-- E già qui dovrebbe vedersi perché non è possibile.
-- Ma proviamo a definire un fold con una map: inizio a scrivere la funzione e mi ritrovo alla fine che la mia map andrà a
-- restituire questa lunga lista di valori, ma io voglio un singolo valore, che ovviamente deve risultare dall'applicazione
-- di una funzione a tutta quanta la lista (per esempio una somma).
-- Come faccio a far "collassare" man mano la lista con l'applicazione della funzione? Semplice, uso foldr o foldl
-- Però non posso usarli... e quindi è impossibile.


belongsTo :: (Eq a) => a -> [a] -> Bool
belongsTo = notElem

main :: IO ()

-- TEST ESERCIZIO 1.1

main = print(belongsTo 5 [5, 3, 5])

-- main = print(shuffle [1, 2, 3] [4, 5] [1, 2, 4, 3, 5])
-- main = print(shuffle [1, 2] [4] [1, 4, 2])
-- main = print(shuffle [7, 1, 5] [8, 3] [7, 8, 1, 3, 5])

-- TEST ESERCIZIO 1.3

-- main = print(shuffle [1, 2, 3] [1, 2] [1, 1, 2, 2, 3])
-- main = print(shuffle [4, 4, 4] [5, 4, 5] [4, 4, 5, 4, 4, 5])
-- main = print(shuffle [2, 2, 5, 2] [6, 2, 2, 5] [2, 6, 2, 2, 2, 5, 2, 5])


-- TEST ESERCIZIO 2 (purtroppo non ho trovato esempi per i test)

-- main = print(segments [1, 2, 3]) 
-- main = print(segments ['A', 'B', 'C', 'D', 'E']) 
-- main = print(segments ["pollo", "pizza", "pasta", "lasagna"])


-- TEST ESERCIZIO 3.1 (presi qui: http://zvon.org/other/haskell/Outputprelude/zipWith_f.html)

-- main = print(myZipWith (+) [1,2,3] [3,2,1])
-- main = print(myZipWith (**) (replicate 10 5) [1..10])
-- main = print(myZipWith (\x y -> 2*x + y) [1..4] [5..8])


-- TEST ESERCIZIO 3.2 (presi qui, sia per il 3.2 che 3.3: http://zvon.org/other/haskell/Outputprelude/map_f.html)

-- main = print(myMap (3*) [1,2,3,4])
-- main = print(myMap abs [-1,-3,4,-12])
-- main = print(myMap reverse ["abc","cda","1234"])
-- main = print(myMap (recip . negate) [1,4,-5,0.1])


-- TEST ESERCIZIO 3.3 

-- main = print(myMap2 (3*) [1,2,3,4])
-- main = print(myMap2 abs [-1,-3,4,-12])
-- main = print(myMap2 reverse ["abc","cda","1234"])
-- main = print(myMap2 (recip . negate) [1,4,-5,0.1])




