module Haskell where

import Data.Char
import Data.List

{-- 
   
   50 Questões desenvolvidas em Haskell.

   Nome:Etienne Costa 
   Email: etienne_costa@hotmail.com
  
  Email disponibilizado para dúvidas e sugestões.


  

--}

{-- 1. 
Apresente uma definição recursiva da função (pré-definida) myEnumFromTo ::
Int -> Int -> [Int] que constrói a lista dos números inteiros compreendidos
entre dois limites. Por exemplo, myEnumFromTo 1 5 corresponde à lista [1,2,3,4,5] --}
myEnumFromTo :: Int -> Int -> [Int]
myEnumFromTo x y | x> y = []
                 | otherwise = x: myEnumFromTo (x+1) y

{-- 2. 
Apresente uma definição recursiva da função (pré-definida) myEnumFromThenTo
:: Int -> Int -> Int -> [Int] que constrói a lista dos números inteiros
compreendidos entre dois limites e espaçados de um valor constante. Por exemplo,
myEnumFromThenTo 1 3 10 corresponde à lista [1,3,5,7,9]. --}
myEnumFromThenTo :: Int -> Int -> Int -> [Int]
myEnumFromThenTo x y z = map toEnum [fromEnum x, fromEnum y .. fromEnum z]

{-- 3. 
Apresente uma definição recursiva da função (pré-definida) (+++) :: [a] ->
[a] -> [a] que concatena duas listas. Por exemplo, (+++) [1,2,3] [10,20,30]
corresponde à lista [1,2,3,10,20,30]. --}
infixl +++
(+++) :: [a] -> [a] -> [a]
(+++) x [] = x 
(+++) [] x = x 
(+++) (h:t) l = h : (+++) t l

{-- 4. 
Apresente uma definição recursiva da função (pré-definida) myLast :: [a] ->
a que calcula o último elemento de uma lista não vazia. Por exemplo, last
[10,20,30] corresponde a 30. --}
myLast :: [a] -> a
myLast [x]= x 
myLast [] = error "Não está definida para listas vazias!"
myLast (h:t) = myLast t 

{-- 5. 
Apresente uma definição recursiva da função (pré-definida) myInit :: [a] ->
[a] que dada uma lista não vazia calcula uma lista igual a essa mas sem o
último elemento. Por exemplo, myInit [10,20,30] corresponde a [10,20]. --}
myInit :: [a] -> [a]
myInit [] = error "Não está definida para listas vazias!"
myInit [x]= []
myInit (h:t) = h: myInit t 
{-- 6. 
Apresente uma definição recursiva da função (pré-definida) (.!!) :: [a] ->
Int -> a que dada uma lista e um inteiro, calcula o elemento da lista que se
encontra nessa posição (assumese que o primeiro elemento se encontra na posição
0. Por exemplo, (.!!) [10,20,30] 1 corresponde a 20. Ignore os casos em que a
função não se encontra definida (i.e., em que a posição fornecida não
corresponde a nenhuma posição válida da lista). --}
infixl .!!
(.!!) :: [a] -> Int -> a
(.!!) [] _  = error "Lista Vazia"
(.!!) (h:t) x | x < 0 = error "Índice negativo."
              | x >= length(h:t) = error "Índice muito grande"
              | x == 0 &&  length (h:t) > 0 = h 
              | otherwise = (.!!) t (x-1)

{-- 7. 
Apresente uma definição recursiva da função (pré-definida) myReverse :: [a]
-> [a] que dada uma lista calcula uma lista com os elementos dessa lista pela
ordem inversa. Por exemplo, myReverse [10,20,30] corresponde a [30,20,10]. --}
myReverse :: [a] -> [a]
myReverse l = aux l []
         where aux [] a = a 
               aux (h:t) a = aux t (h:a)

{-- 8. 
Apresente uma definição recursiva da função (pré-definida) myTake :: Int ->
[a] -> [a] que dado um inteiro n e uma lista l calcula a lista com os (no
máximo) n primeiros elementos de l.
A lista resultado só terá menos de que n elementos se a lista l tiver menos do
que n elementos. Nesse caso a lista calculada é igual à lista fornecida. Por
exemplo, myTake 2 [10,20,30] corresponde a [10,20]. --}
myTake :: Int -> [a] -> [a]
myTake x [] = [] 
myTake x _ | x <= 0 = []
myTake x (h:t) = h : myTake (x-1) t

{-- 9. 
Apresente uma definição recursiva da função (pré-definida) myDrop :: Int ->
[a] -> [a] que dado um inteiro n e uma lista l calcula a lista sem os (no
máximo) n primeiros elementos de l. Se a lista fornecida tiver n elementos ou
menos, a lista resultante será vazia. Por exemplo, myDrop 2 [10,20,30] 
corresponde a [30]. --}
myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop 0 (h:t) = (h:t)
myDrop n (h:t) | n>0= myDrop (n-1) t 
               | otherwise = (h:t)



{-- 10. 
Apresente uma definição recursiva da função (pré-definida) myZip :: [a] ->
[b] -> [(a,b)] constói uma lista de pares a partir de duas listas. Por exemplo,
myZip [1,2,3] [10,20,30,40] corresponde a [(1,10),(2,20),(3,30)]. --}
myZip :: [a] -> [b] -> [(a,b)] 
myZip [] _ = []
myZip _ [] = []
myZip (h:t) (x:xs) = (h,x): myZip t xs 

{-- 11. 
Apresente uma definição recursiva da função (pré-definida) myElem :: Eq a
=> a -> [a] -> Bool que testa se um elemento ocorre numa lista. Por exemplo,
elem 20 [10,20,30] corresponde a True enquanto que myElem 2 [10,20,30] 
corresponde a False. --}
myElem :: Eq a => a -> [a] -> Bool
myElem x [] = False
myElem x (h:t) | x==h = True 
               | otherwise = myElem x t 

{-- 12. 
Apresente uma definição recursiva da função (pré-definida) myReplicate ::
Int -> a -> [a] que dado um inteiro n e um elemento x constói uma lista com n
elementos, todos iguais a x. Por exemplo, myReplicate 3 10 corresponde a
[10,10,10]. --}
myReplicate :: Int -> a -> [a] 
myReplicate 0 _ = []
myReplicate x a | x <0 = []
                | otherwise = a : myReplicate (x-1) a 

{-- 13. 
Apresente uma definição recursiva da função (pré-definida) myIntersperse
:: a -> [a] -> [a] que dado um elemento e uma lista, constrói uma lista em que o
elemento fornecido é intercalado entre os elementos da lista fornecida. Por
exemplo, myIntersperse 1 [10,20,30] corresponde a [10,1,20,1,30]. --}
myIntersperse :: a -> [a] -> [a]
myIntersperse x [] =[]
myIntersperse _ [x]=[x]
myIntersperse x (h:t) = h:x:myIntersperse x t 

{-- 14. 
Apresente uma definição recursiva da função (pré-definida) myGroup :: Eq a
=> [a] -> [[a]] que agrupa elementos iguais e consecutivos de uma lista. Por
exemplo, myGroup [1,2,2,3,4,4,4,5,4] corresponde a
[[1],[2,2],[3],[4,4,4],[5],[4]]. --}
myGroup :: Eq a => [a] -> [[a]]
myGroup [] =[]
myGroup [x]=[[x]]
myGroup (h:t) = [groupp h (h:t)] ++ myGroup(auxiliar h (h:t) )   

groupp::Eq a => a ->[a]->[a]
groupp x [] = []
groupp x (h:t) | x ==h = x:groupp x t 
               | otherwise = []

auxiliar::Eq a => a -> [a]->[a]
auxiliar x [] =[]
auxiliar x (h:t) | x==h = auxiliar x t 
                 | otherwise = (h:t)
{-- 15. 
Apresente uma definição recursiva da função (pré-definida) myConcat ::
[[a]] -> [a] que concatena as listas de uma lista. Por exemplo, myConcat
[[1],[2,2],[3],[4,4,4],[5],[4]] corresponde a [1,2,2,3,4,4,4,5,4]. --}
myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (h:t) = h ++ myConcat t

{-- 16. 
Apresente uma definição recursiva da função (pré-definida) myInits :: [a]
-> [[a]] que calcula a lista dos prefixos de uma lista. Por exemplo, myInits
[11,21,13] corresponde a [[],[11],[11,21],[11,21,13]]. --}
myInits :: [a] -> [[a]]
myInits []=[[]]
myInits l=  myInits(init l)  ++ [l]

{-- 17. 
Apresente uma definição recursiva da função (pré-definida) myTails :: [a]
-> [[a]] que calcula a lista dos sufixos de uma lista. Por exemplo, myTails
[1,2,3] corresponde a [[1,2,3],[2,3],[3],[]]. --}
myTails :: [a] -> [[a]]
myTails []=[[]]
myTails l = [l] ++ myTails(tail l)

{-- 18. 
Apresente uma definição recursiva da função (pré-definida) myIsPrefixOf ::
Eq a => [a] -> [a] -> Bool que testa se uma lista é prefixo de outra. Por
exemplo, isPrefixOf [10,20] [10,20,30] corresponde a True enquanto que
myIsPrefixOf [10,30] [10,20,30] corresponde a False. --}
myIsPrefixOf :: Eq a => [a] -> [a] -> Bool 
myIsPrefixOf _ [] = False
myIsPrefixOf [] _ = True
myIsPrefixOf (h:t) (x:xs) | h==x = myIsPrefixOf t xs  
                          | otherwise = False


                          
{-- 19. 
Apresente uma definição recursiva da função (pré-definida) myIsSuffixOf ::
Eq a => [a] -> [a] -> Bool que testa se uma lista é sufixo de outra. Por
exemplo, isSuffixOf [20,30] [10,20,30] corresponde a True enquanto que
myIsSuffixOf [10,30] [10,20,30] corresponde a False. --}
myIsSuffixOf :: Eq a => [a] -> [a] -> Bool 
myIsSuffixOf _ [] = False
myIsSuffixOf [] _ = True
myIsSuffixOf x y  | last x == last y = myIsSuffixOf (init x) (init y)
                  | otherwise = False

{-- 20. 
Apresente uma definição recursiva da função (pré-definida)
myIsSubsequenceOf :: Eq a => [a] -> [a] -> Bool que testa se os elementos de uma
lista ocorrem noutra pela mesma ordem relativa. Por exemplo, myIsSubsequenceOf
[20,40] [10,20,30,40] corresponde a True enquanto que myIsSubsequenceOf [40,20]
[10,20,30,40] corresponde a False. --}
myIsSubsequenceOf :: Eq a => [a] -> [a] -> Bool 
myIsSubsequenceOf _ _ = undefined -- definir esta função

{-- 21. 
Apresente uma definição recursiva da função (pré-definida) myElemIndices
:: Eq a => a -> [a] -> [Int] que calcula a lista de posições em que um dado
elemento ocorre numa lista. Por exemplo, myElemIndices 3 [1,2,3,4,3,2,3,4,5]
corresponde a [2,4,6]. --}
myElemIndices :: Eq a => a -> [a] -> [Int]
myElemIndices x [] =[]
myElemIndices x l= indice x l 0

indice::Eq a => a -> [a]->Int ->[Int]
indice x [] _ = []
indice x (h:t) n | x==h = n:indice x t (n+1)
                 | otherwise = indice x t (n+1)

{-- 22. 
Apresente uma definição recursiva da função (pré-definida) myNub :: Eq a
=> [a] -> [a] que calcula uma lista com os mesmos elementos da recebida, sem
repetições. Por exemplo, myNub [1,2,1,2,3,1,2] corresponde a [1,2,3]. --}
myNub :: Eq a => [a] -> [a]
myNub [] = []
myNub (h:t)  = h : myNub(remove h t)

{-- 23. 
Apresente uma definição recursiva da função (pré-definida) myDelete :: Eq
a => a -> [a] -> [a] que retorna a lista resultante de remover (a primeira
ocorrência de) um dado elemento de uma lista. Por exemplo, myDelete 2
[1,2,1,2,3,1,2] corresponde a [1,1,2,3,1,2]. Se não existir nenhuma ocorrência
a função deverá retornar a lista recebida. --}
myDelete :: Eq a => a -> [a] -> [a]
myDelete x [] =[]
myDelete x (h:t) | x == h = t 
                 | otherwise = h : myDelete x t 

{-- 24. 
Apresente uma definição
recursiva da função (pré-definida) (\\\):: Eq a => [a] -> [a] -> [a] que retorna
a lista resultante de remover (as primeiras ocorrências) dos elementos da
segunda lista da primeira. Por exemplo, (\\)[1,2,3,4,5,1] [1,5] corresponde a
[2,3,4,1]. --}
infixl \\\
(\\\) :: Eq a => [a] -> [a] -> [a]
(\\\)  x [] = x
(\\\) [] _ = []
(\\\)  l (h:t) = (\\\) (myDelete h l) t 




{-- 25. 
Apresente uma definição recursiva da função (pré-definida) myUnion :: Eq a
=> [a] -> [a] -> [a] que retorna a lista resultante de acrescentar à primeira
lista os elementos da segunda que não ocorrem na primeira. Por exemplo, myUnion
[1,1,2,3,4] [1,5] corresponde a [1,1,2,3,4,5]. --}
myUnion :: Eq a => [a] -> [a] -> [a] 
myUnion x [] = x 
myUnion [] x = x
myUnion l (h:t) | elem h l == False = myUnion (l++[h]) t  
                | otherwise = myUnion l t 


{-- 26. 
Apresente uma definição recursiva da função (pré-definida) myIntersect ::
Eq a => [a] -> [a] -> [a] que retorna a lista resultante de remover da primeira
lista os elementos que não pertencem à segunda. Por exemplo, myIntersect
[1,1,2,3,4] [1,3,5] corresponde a [1,1,3]. --}
myIntersect :: Eq a => [a] -> [a] -> [a]
myIntersect _ [] = []
myIntersect [] _ = [] 
myIntersect (h:t) l | elem h l == True = h : myIntersect t l 
                    | otherwise = myIntersect t l 

{-- 27. 
Apresente uma definição recursiva da função (pré-definida) myInsert :: Ord
a => a -> [a] -> [a] que dado um elemento e uma lista ordenada retorna a lista
resultante de inserir ordenadamente esse elemento na lista. Por exemplo, 
myInsert 25 [1,20,30,40] corresponde a [1,20,25,30,40]. --}
myInsert :: Ord a => a -> [a] -> [a]
myInsert x [] =[x]
myInsert x (h:t) | x <=h =(x:h:t)
                 | otherwise = h: myInsert x t 

{-- 28. 
Apresente uma definição recursiva da função (pré-definida) myMaximum ::
Ord a => [a] -> a que dada uma lista não vazia retorna o maior elemento da
lista. Por exemplo, myMaximum [10,50,3,40] corresponde a 50. --}
myMaximum :: Ord a => [a] -> a
myMaximum [x]=x
myMaximum (h:t) | (myMaximum t) > h = myMaximum t 
                | otherwise = h 
{-- 29. 
Apresente uma definição recursiva da função (pré-definida) myMinimum ::
Ord a => [a] -> a que dada uma lista não vazia retorna o menor elemento da
lista. Por exemplo, myMinimum [10,50,3,40] corresponde a 3. --}
myMinimum :: Ord a => [a] -> a
myMinimum [x]=x
myMinimum (h:t) | (myMinimum t) > h = myMinimum t 
                | otherwise = h 

{-- 30. 
Apresente uma definição recursiva da função (pré-definida) mySum :: Num a
=> [a] -> a que dada uma lista retorna a soma dos seus elementos. Por exemplo,
mySum [10,50,3,40] corresponde a 103. --}
mySum :: Num a => [a] -> a
mySum [] = 0
mySum (h:t) = h + mySum t 

{-- 31. 
Apresente uma definição recursiva da função (pré-definida) myProduct ::
Num a => [a] -> a que dada uma lista retorna o produto dos seus elementos. Por
exemplo, myProduct [10,50,3,40] corresponde a 60000. --}
myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (h:t) = h * myProduct t 

{-- 32. 
Apresente uma definição recursiva da função (pré-definida) myAnd :: [Bool]
-> Bool que dada uma lista retorna True se todos os elementos da lista forem
True. --}
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (h:t) = h && myAnd t 

{-- 33. 
Apresente uma definição recursiva da função (pré-definida) myOr :: [Bool]
-> Bool que dada uma lista retorna True se pelo menos um dos elementos da lista
for True. --}
myOr :: [Bool] -> Bool
myOr [] = False 
myOr (h:t) = h || myOr t 

{-- 34. 
Apresente uma definição recursiva da função (pré-definida) myUnwords ::
[String] -> String que junta todas as strings da lista numa só, separando-as por
um espaço. Por exemplo, myUnwords ["Programacao", "Funcional"] corresponde a
"Programacao Funcional". --}
myUnwords :: [String] -> String
myUnwords [] =[]
myUnwords (h:t) = h ++ " "++ myUnwords t 

{-- 35. 
Apresente uma definição recursiva da função (pré-definida) myUnlines ::
[String] -> String que junta todas as strings da lista numa só, separando-as
pelo caracter ’\n’. Por exemplo, myUnlines ["Prog", "Func"] corresponde a
"Prog\nFunc". --}
myUnlines :: [String] -> String
myUnlines []=[]
myUnlines [x]=x
myUnlines (h:t) = h ++ "\n" ++ myUnlines t 

{-- 36. 
Apresente uma definição recursiva da função pMaior :: Ord a => [a] ->
Int que dada uma lista não vazia, retorna a posição onde se encontra o maior
elemento da lista. As posições da lista começam em 0, i.e., a função deverá
retornar 0 se o primeiro elemento da lista for o maior. --}
pMaior :: Ord a => [a] -> Int
pMaior l = pMaiorAux l 0


pMaiorAux::Ord a => [a] -> Int -> Int 
pMaiorAux [x] _ = 0
pMaiorAux (h:t) x | h == myMaximum (h:t) = x 
                  | otherwise = pMaiorAux t (x+1)

{-- 37. 
Apresente uma definição recursiva da função temRepetidos :: Eq a => [a]
-> Bool que testa se uma lista tem elementos repetidos. Por exemplo,
temRepetidos [11,21,31,21] corresponde a True enquanto que temRepetidos
[11,2,31,4] corresponde a False. --}
temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos [x] = False
temRepetidos (h:t) = elem h t || temRepetidos t 

{-- 38. 
Apresente uma definição recursiva da função algarismos :: [Char] ->
[Char] que determina a lista dos algarismos de uma dada lista de caracteres. Por
exemplo, algarismos "123xp5" corresponde a "1235". --}
algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (h:t) | isDigit h = h : algarismos t 
                 | otherwise = algarismos t 

{-- 39. 
Apresente uma definição recursiva da função posImpares :: [a] -> [a] que
determina os elementos de uma lista que ocorrem em posições ímpares. Considere
que o primeiro elemento da lista ocorre na posição 0 e por isso par. Por
exemplo, posImpares [10,11,7,5] corresponde a [11,5]. --}
posImpares :: [a] -> [a]
posImpares[x]=[]
posImpares []=[]
posImpares (h:x:t) = x : posImpares t 


{-- 40. 
Apresente uma definição recursiva da função posPares :: [a] -> [a] que
determina os elementos de uma lista que ocorrem em posições ímpares. Considere
que o primeiro elemento da lista ocorre na posição 0 e por isso par. Por
exemplo, posPares [10,11,7,5] corresponde a [10,7]. --} --}
posPares :: [a] -> [a]
posPares [x] = [x]
posPares [] =[]
posPares (h:x:t)= h : posPares t

{-- 41. 
Apresente uma definição recursiva da função isSorted :: Ord a => [a] ->
Bool que testa se uma lista está ordenada por ordem crescente. Por exemplo,
isSorted [1,2,2,3,4,5] corresponde a True, enquanto que isSorted [1,2,4,3,4,5]
corresponde a False. --}
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted (h:t) | h == myMinimum (h:t) = isSorted t 
               | otherwise = False

{-- 42. 
Apresente uma definição recursiva da função iSort :: Ord a => [a] -> [a]
que calcula o resultado de ordenar uma lista. Assuma, se precisar, que existe
definida a função insert
:: Ord a => a -> [a] -> [a] que dado um elemento e uma lista ordenada retorna a
:: lista
resultante de inserir ordenadamente esse elemento na lista. --}
iSort :: Ord a => [a] -> [a]
iSort [] =[]
iSort (h:t) = myInsert h (iSort t )

{-- 43. 
Apresente uma definição recursiva da função menor :: String -> String ->
Bool que dadas duas strings, retorna True se e só se a primeira for menor do que
a segunda, segundo a ordem lexicográfica (i.e., do dicionário) Por exemplo,
menor "sai" "saiu" corresponde a True enquanto que menor "programacao"
"funcional" corresponde a False. --}
menor :: String -> String -> Bool
menor [] _ = True
menor _ [] = False
menor (h:t) (x:xs) = h<=x && menor t xs 

{-- 44. 
Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos
de elementos de a. Considere ainda que nestas listas não há pares cuja primeira
componente coincida, nem cuja segunda componente seja menor ou igual a zero.
Defina a função elemMSet :: Eq a => a -> [(a,Int)] -> Bool que testa se um
elemento pertence a um multi-conjunto. Por exemplo, elemMSet ’a’ [(’b’,2),
(’a’,4), (’c’,1)] corresponde a True enquanto que elemMSet ’d’ [(’b’,2),
(’a’,4), (’c’,1)] corresponde a False. --}
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet x [] = False
elemMSet x ((a,b):t) | x==a = True 
                     | otherwise = elemMSet x t 

{-- 45. 
Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos
de elementos de a. Considere ainda que nestas listas não há pares cuja primeira
componente coincida, nem cuja segunda componente seja menor ou igual a zero.
Defina a função lengthMSet :: [(a,Int)] -> Int que calcula o tamanho de um
multiconjunto. Por exemplo, lengthMSet [(’b’,2), (’a’,4), (’c’,1)] corresponde a
7. --}
lengthMSet :: [(a,Int)] -> Int
lengthMSet []=0
lengthMSet ((a,b):t) = b+ lengthMSet t 

{-- 46. 
Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos
de elementos de a. Considere ainda que nestas listas não há pares cuja primeira
componente coincida, nem cuja segunda componente seja menor ou igual a zero.
Defina a função converteMSet :: [(a,Int)] -> [a] que converte um multi-conjuto
na lista dos seus elementos
Por exemplo, converteMSet [(’b’,2), (’a’,4), (’c’,1)] corresponde a "bbaaaac". --}
converteMSet :: [(a,Int)] -> [a]
converteMSet [] =[] 
converteMSet ((a,b):t) = myReplicate b a ++ converteMSet t 

{-- 47. 
Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos
de elementos de a. Considere ainda que nestas listas não há pares cuja primeira
componente coincida, nem cuja segunda componente seja menor ou igual a zero.
Defina a função insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] que acrescenta
um elemento a um multi-conjunto. Por exemplo, insereMSet ’c’ [(’b’,2), (’a’,4),
(’c’,1)] corresponde a [(’b’,2), (’a’,4), (’c’,2)]. --}
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet x [] = [(x,1)]
insereMSet x  ((a,b):t) | x==a = ((a,b+1):t)
                        | otherwise = (a,b): insereMSet x t 

{-- 48. 
Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos
de elementos de a. Considere ainda que nestas listas não há pares cuja primeira
componente coincida, nem cuja segunda componente seja menor ou igual a zero.
Defina a função removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] que remove um
elemento a um multi-conjunto. Se o elemento não existir, deve ser retornado o
multi-conjunto recebido. Por exemplo, removeMSet ’c’ [(’b’,2), (’a’,4), (’c’,1)]
corresponde a [(’b’,2), (’a’,4)]. --}
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet x [] =[]
removeMSet x ((a,b):t) | x==a && (b-1 == 0) = t 
                       | x==a =((a,b-1):t)
                       | otherwise = (a,b) : removeMSet x t  

{-- 49. 
Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos
de elementos de a. Considere ainda que nestas listas não há pares cuja primeira
componente coincida, nem cuja segunda componente seja menor ou igual a zero.
Defina a função constroiMSet :: Ord a => [a] -> [(a,Int)] dada uma lista
ordenada por ordem crescente, calcula o multi-conjunto dos seus elementos. Por
exemplo, constroiMSet "aaabccc" corresponde a [(’a’,3), (’b’,1), (’c’,3)]. --}
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = (h,quantos h (h:t)): constroiMSet (remove h (h:t))

quantos::Eq a => a -> [a]->Int
quantos x [] =0
quantos x (h:t) | x==h = 1 + quantos x t 
                | otherwise = quantos x t  

remove:: Eq a => a -> [a] -> [a]
remove x [] = []
remove x (h:t) | x==h = remove x t 
               | otherwise = h: remove x t 





{-- 50. 
Apresente uma definição recursiva da função somaPares :: [Int] -> Int
que soma os números pares de uma lista de inteiros. Por exemplo, somaPares
[2,4,5,3,4,2] corresponde a 12. --}
somaPares :: [Int] -> Int
somaPares [] = 0
somaPares (h:t) | (mod h 2)==0  = h + somaPares t 
                |  otherwise = somaPares t 