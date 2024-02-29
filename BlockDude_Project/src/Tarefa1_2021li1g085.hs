{- |
Module      : Tarefa1_2021li1g085
Description : Validação de um potencial mapa
Copyright   : LingYun Zhu <a100820@alunos.uminho.pt>;
            : Ana Margarida Sousa Pimenta <a100830@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g085 where

import LI12122

{- | A função 'validaPotencialMapa' junta funções secundárias de modo a poder validar o mapa.

Esta é definida da seguinte forma:

@
validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = False
validaPotencialMapa a = vPecaPorPosicao x xs && unicaPorta a 0 && nflutua a && existeEspVazio a && blocoPisoChao a
    where
     (x:xs) = pecaCoorEmCoor a
@

== Exemplos de utilização:

>>> validaPotenciaMapa m1
True
-}

validaPotencialMapa :: [(Peca, Coordenadas)] -- ^ argumento 
                        -> Bool              -- ^ validade do Mapa

validaPotencialMapa [] = False
validaPotencialMapa a = vPecaPorPosicao x xs && unicaPorta a 0 && nflutua a && existeEspVazio a && blocoPisoChao a
    where
     (x:xs) = pecaCoorEmCoor a



{- | A função 'pecaCoorEmCoor' transforma o argumento para uma lista de 'Coordenadas', de modo a poderem ser comparadas.

@
pecaCoorEmCoor :: [(Peca, Coordenadas)] -> [Coordenadas]
pecaCoorEmCoor [] = []
pecaCoorEmCoor ((_,(b,c)):t) = (b,c) : pecaCoorEmCoor t
@

== Exemplos de utilização:

>>> pecaCoorEmCoor [(Bloco,(0,2)),(Caixa,(3,3)),(Porta,(0,2))]
[(0,2),(3,3),(0,2)]
-}

pecaCoorEmCoor :: [(Peca, Coordenadas)] -- ^ lista de 'Peca' e 'Coordenadas'
                  -> [Coordenadas]      -- ^ lista de 'Coordenadas'
    
pecaCoorEmCoor [] = []
pecaCoorEmCoor ((_,(b,c)):t) = (b,c) : pecaCoorEmCoor t


{- | A função 'vPecaPorPosicao' recorre à função 'pecaCoorEmCoor' para verificar se existem duas peças a ocupar a mesma posição.

@
vPecaPorPosicao :: Coordenadas -> [Coordenadas] -> Bool 
vPecaPorPosicao _ [] = True
vPecaPorPosicao a (b:t) | elem a (b:t) = False 
                        | otherwise = vPecaPorPosicao b t
@
-}

vPecaPorPosicao :: Coordenadas -> [Coordenadas] -> Bool 
vPecaPorPosicao _ [] = True
vPecaPorPosicao a (b:t) | a `elem` (b:t) = False 
                        | otherwise = vPecaPorPosicao b t



{- | A função 'unicaPorta' soma 1 ao acumulador sempre que aparece Porta na lista. 

@
unicaPorta :: [(Peca,Coordenadas)] -> Int -> Bool 
unicaPorta [] ac | ac == 0 = False
                 | otherwise = True
unicaPorta ((a,(b,c)):t) ac | ac > 1 = False
                            | a == Porta = unicaPorta t (ac+1)
                            | otherwise  = unicaPorta t ac
@

== Exemplos de utilização:

>>> unicaPorta [(Porta,(0,0)),(Porta,(1,0))]
False

>>> unicaPorta m1
True
-}

unicaPorta :: [(Peca,Coordenadas)] -> Int -> Bool 
unicaPorta [] ac | ac == 0 = False
                 | otherwise = True
unicaPorta ((a,(b,c)):t) ac | ac > 1 = False
                            | a == Porta = unicaPorta t (ac+1)
                            | otherwise  = unicaPorta t ac

{- | A função 'nflutua' certifica de que não há nenhuma 'Caixa' a flutuar.

-- | Recorre-se a 'procuraCaixa'

@
nflutua :: [(Peca,Coordenadas)] -> Bool 
nflutua [] = True 
nflutua l = procuraCaixa l l
@
-}

nflutua :: [(Peca,Coordenadas)] -- ^ lista
            -> Bool             -- ^ inexistência de 'Caixa' a flutuar

nflutua [] = True 
nflutua l = procuraCaixa l l

{- |
 -- * Quando se encontra uma 'Caixa' na lista, procura-se a base da mesma.

@
procuraCaixa :: [(Peca,Coordenadas)]-> [(Peca,Coordenadas)] -> Bool 
procuraCaixa [] _ = True
procuraCaixa ((a,b):t) l | a == Caixa = procuraBase (a,b) l && procuraCaixa t l
                         | otherwise  = procuraCaixa t l
@
-}

procuraCaixa :: [(Peca,Coordenadas)]    -- ^ lista
                -> [(Peca,Coordenadas)] -- ^ lista inicial completa
                -> Bool                 -- ^ inexistência de 'Caixa' a flutuar

procuraCaixa [] _ = True
procuraCaixa ((a,b):t) l | a == Caixa = procuraBase (a,b) l && procuraCaixa t l
                         | otherwise  = procuraCaixa t l

{- |
-- * Verifica a existencia de uma 'Caixa' ou 'Bloco' numa coordenada a baixo do eixo y.

@
procuraBase :: (Peca,Coordenadas) -> [(Peca,Coordenadas)] -> Bool 
procuraBase _ [] = False
procuraBase (n,(x,y)) ((p,(a,b)):ts) | x == a && (y+1) == b && p == Bloco = True
                                     | x == a && (y+1) == b && p == Caixa = True
                                     | otherwise = procuraBase (n,(x,y)) ts
@
-}
procuraBase :: (Peca,Coordenadas)       -- ^ 'Coordenadas' de 'Caixa'
                -> [(Peca,Coordenadas)] -- ^ lista original com todos os elementos
                -> Bool                 -- ^ inexistência de 'Caixa' a flutuar

procuraBase _ [] = False
procuraBase (n,(x,y)) ((p,(a,b)):ts) | x == a && (y+1) == b && p == Bloco = True
                                     | x == a && (y+1) == b && p == Caixa = True
                                     | otherwise = procuraBase (n,(x,y)) ts

{- | 
A função 'dimensaoMatriz' busca as maiores coordenadas da lista, e soma se 1 no fim a cada eixo, de modo a compensar as coordenadas nulas.

@
dimensaoMatriz :: [(Peca,Coordenadas)] -> (Coordenadas)
dimensaoMatriz [] = (0,0)
dimensaoMatriz [(a,(c,d))] = (c+1,d+1)
dimensaoMatriz ((p1,(a1,b1)):(p2,(a2,b2)):t) | a1 >= a2 && b1 >= b2 = dimensaoMatriz ((p1,(a1,b1)):t)
                                             | b1 > b2   = dimensaoMatriz ((p1,(a2,b1)):t)
                                             | a1 > a2   = dimensaoMatriz ((p1,(a1,b2)):t)
                                             | otherwise = dimensaoMatriz ((p1,(a2,b2)):t)
@
-}


dimensaoMatriz :: [(Peca,Coordenadas)] -- ^ argumento
                   -> Coordenadas    -- ^ dimensão da 'Matriz'
dimensaoMatriz [] = (0,0)
dimensaoMatriz [(a,(c,d))] = (c+1,d+1)
dimensaoMatriz ((p1,(a1,b1)):(p2,(a2,b2)):t) | a1 >= a2 && b1 >= b2 = dimensaoMatriz ((p1,(a1,b1)):t)
                                             | b1 > b2   = dimensaoMatriz ((p1,(a2,b1)):t)
                                             | a1 > a2   = dimensaoMatriz ((p1,(a1,b2)):t)
                                             | otherwise = dimensaoMatriz ((p1,(a2,b2)):t)

{- | Quando existe 'Peca' que seja 'Vazio' a função devolve 'True'. 

@
espacoVazio :: [(Peca,Coordenadas)] -> Bool
espacoVazio [] = False
espacoVazio ((a,(b,c)):t) | a == Vazio = True 
                          | otherwise = espacoVazio t
@
-}

espacoVazio :: [(Peca,Coordenadas)] -- ^ lista de elementos do 'Mapa'
               -> Bool              -- ^ existencia de 'Peca' que seja 'Vazio'

espacoVazio [] = False
espacoVazio ((a,(b,c)):t) | a == Vazio = True 
                          | otherwise = espacoVazio t
 
{- | Caso não exista 'Peca' que seja 'Vazio', a função 'existeEspVazio' recorre à comparação da dimensão da 'Matriz' com o número de elementos que esta possui.

@
existeEspVazio :: [(Peca,Coordenadas)] -> Bool
existeEspVazio [] = False
existeEspVazio l | a == False = length l < (x*y)
                 | a == True  = True
   where 
       (x,y) = dimensaoMatriz l
       a     = espacoVazio l
@
-}

existeEspVazio :: [(Peca,Coordenadas)]  -- ^ lista de elementos
                  -> Bool               -- ^ existência de espaço vazio

existeEspVazio [] = False
existeEspVazio l | a == True  = True
                 | a == False = length l < (x*y) 
                 | otherwise  = False
                  
   where 
       (x,y) = dimensaoMatriz l
       a     = espacoVazio l

{- | A função 'soBlocos' gera uma lista so com as cooredenadas de blocos.

@
soBlocos :: [(Peca,Coordenadas)] -> [Coordenadas]
soBlocos [] = []
soBlocos ((a,(b,c)):t) | a == Bloco = (b,c): soBlocos t
                       | otherwise = soBlocos t
@
-}

soBlocos :: [(Peca,Coordenadas)] -- ^ elementos do 'Mapa'
            -> [Coordenadas]     -- ^ coordenadas dos elementos

soBlocos [] = []
soBlocos ((a,(b,c)):t) | a == Bloco = (b,c): soBlocos t
                       | otherwise = soBlocos t

{- | A função 'firstBloco' procura pelo 'Bloco' mais a baixo e mais a 'Oeste' da lista.

@
firstBloco :: [Coordenadas] -> Coordenadas -> Coordenadas
firstBloco [] ac = ac
firstBloco ((b,c):t) (x,y) | b == 0 && c > y = firstBloco t (b,c)
                           | otherwise = firstBloco t (x,y)
@
-}

firstBloco :: [Coordenadas]  -- ^ lista só de 'Bloco'
              -> Coordenadas -- ^ assume-se a existência de 'Bloco' na coluna 0 
              -> Coordenadas -- ^ bloco com maior y (situado mais a baixo)

firstBloco [] ac = ac
firstBloco ((b,c):t) (x,y) | b == 0 && c > y = firstBloco t (b,c)
                           | otherwise = firstBloco t (x,y)




{- | Esta função ordena as coordenadas de baixo para cima, de esquerda para a direira, utilizando o algoritmo 'quick sort'. 

@
quickBlocos :: [Coordenadas] -> [Coordenadas]
quickBlocos [] = []
quickBlocos (x:xs) = (quickBlocos a1) ++ [x] ++ (quickBlocos a2)

    where
        (a1,a2) = parteBlocos x xs
@
-}
quickBlocos :: [Coordenadas]   -- ^ 'Coordenadas' de 'Bloco'
              -> [Coordenadas] -- ^ 'Coordenadas' ordenadas

quickBlocos [] = []
quickBlocos (x:xs) = quickBlocos a1 ++ [x] ++ quickBlocos a2

    where
        (a1,a2) = parteBlocos x xs

{- | A função divide os elementos da lista em duas listas recorrendo a 'Tupling':

-- * elementos mais a 'Baixo' e a 'Esquerda'
-- * elementos mais a 'Cima' e a 'Direita' 

@
parteBlocos :: Coordenadas -> [Coordenadas] -> ([Coordenadas],[Coordenadas])
parteBlocos _ [] = ([],[])
parteBlocos (b,c) ((y,z):t) | b > y || (b == y && c < z) = ((y,z):as,bs)
                            | otherwise = (as,(y,z):bs)
    where
        (as,bs) = parteBlocos (b,c) t
@
-}
parteBlocos :: Coordenadas                    -- ^ 'Coordenada' que é utilizada para comparar
             -> [Coordenadas]                 -- ^ lista que vai ser ordenada
             -> ([Coordenadas],[Coordenadas]) -- ^ listas de 'Coordenadas' maiores e menores à de comparação
parteBlocos _ [] = ([],[])
parteBlocos (b,c) ((y,z):t) | b > y || (b == y && c < z) = ((y,z):as,bs)
                            | otherwise = (as,(y,z):bs)
    where
        (as,bs) = parteBlocos (b,c) t


{- | A função 'blocoPiso' recebe as coordenadas ordenadas e utiliza um acumulador para comparar a corrente coordenada com a anterior.

@
blocoPiso :: [Coordenadas] -> Coordenadas -> Coordenadas -> Bool
blocoPiso [] _ _ = False
blocoPiso ((h,z):ts) (a,b) (x,y) | a == x = True
                                 | h < a      = blocoPiso ts (a,b) (x,y)
                                 | z == (b+1) = blocoPiso ts (h,z) (x,y)
                                 | z == b     = blocoPiso ts (h,z) (x,y)
                                 | z == (b-1) = blocoPiso ts (h,z) (x,y)
@
-}

blocoPiso :: [Coordenadas]  -- ^ lista de 'Coordenadas' de 'Bloco'
             -> Coordenadas -- ^ recebe o 'Bloco' mais a 'Este' e 'Sul'
             -> Coordenadas -- ^ recebe a dimensão da 'Matriz'
             -> Bool        -- ^ veracidade da existência de um chão contínuo

blocoPiso [] _ _ = False
blocoPiso ((h,z):ts) (a,b) (x,y) | a == x = True
                                 | h < a      = blocoPiso ts (a,b) (x,y)
                                 | z == (b+1) = blocoPiso ts (h,z) (x,y)
                                 | z == b     = blocoPiso ts (h,z) (x,y)
                                 | z == (b-1) = blocoPiso ts (h,z) (x,y)
blocoPiso _ _ _ = False

{- | A função 'blocoPisoChao' é utilizada para unir as funções definidas anteriormente.

@
blocoPisoChao :: [(Peca,Coordenadas)] -> Bool 
blocoPisoChao l = blocoPiso td (a,b) (x-1,y-1) 
    where
        (a,b) = firstBloco coor (0,0)
        (x,y) = dimensaoMatriz l
        coor  = soBlocos l
        td    = quickBlocos coor
@

-}

blocoPisoChao :: [(Peca,Coordenadas)] -- ^ lista dos elementos do 'Mapa'
                 -> Bool              -- ^ veracidade da existência de um chão contínuo
blocoPisoChao l = blocoPiso td (a,b) (x-1,y-1) 
    where
        (a,b) = firstBloco coor (0,0)
        (x,y) = dimensaoMatriz l
        coor  = soBlocos l
        td    = quickBlocos coor
