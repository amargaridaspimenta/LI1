{- |
Module      : Tarefa6_2021li1gXXX
Description : Resolução de um puzzle

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2021/22.
-}
module Tarefa6_2021li1g085 where

import LI12122
import Tarefa4_2021li1g085
import Data.List

-- A função determina as coordenadas na 'Porta' a partir de duas funções auxiliares.
coorPorta :: Mapa -> Coordenadas
coorPorta mapa = (x,y)
        where
        y = yPorta mapa
        x = xPorta mapa y

--Função auxiliar que determina a abcissa da 'Porta' 
xPorta :: Mapa -> Int -> Int
xPorta [] _     = error "1" -- assumindo que o mapa é válido
xPorta ([]:t) _ = error "2" -- assumindo que o mapa é válido
xPorta ((x:xs):y) n | n == 0 && x == Porta = 0
                    | n == 0 && x /= Porta = 1 + xPorta (xs:y) n
                    | n > 0  = xPorta y (n-1)
                    | otherwise = error "3" -- assumindo que o mapa é válido


--Função que determina a ordenada da 'Porta'
yPorta :: Mapa -> Int
yPorta [] = error "4"     -- assumindo que o mapa é válido
yPorta ([]:t) = error "5" -- assumindo que o mapa é válido
yPorta (x:xs) | Porta `elem` x = 0
              | otherwise  = 1 + yPorta xs

--Determina a 'Peca' que está nas coordenadas inseridas.
pecaNaCoor :: Coordenadas -> Mapa -> Peca
pecaNaCoor _ []     = Bloco
pecaNaCoor (x,y) ([]:t) = pecaNaCoor (x,y-1) t
pecaNaCoor (x,y) ((x0:x1):y0) | y > 0 = pecaNaCoor (x,y-1) y0
                              | x == 0 && y == 0 = x0
                              | y == 0 && x >  0 = pecaNaCoor (x-1,y) (x1:y0)
                              | otherwise = Bloco -- Não é Bloco mas impede o jogador de fazer o movimento

--Função que determina se um movimento do 'Jogador' altera o seu estado.
moveValido :: Jogo -> Movimento -> Bool
moveValido (Jogo mapa (Jogador (x,y) direcao box)) mov = case mov of
                                                              AndarEsquerda -> pecaNaCoor (x-1,y) mapa == Vazio || pecaNaCoor (x-1,y) mapa == Porta -- && (pecaNaCoor (x-1,y+1) mapa == Bloco || pecaNaCoor (x-1,y+1) mapa == Caixa)
                                                              AndarDireita  -> pecaNaCoor (x+1,y) mapa == Vazio || pecaNaCoor (x+1,y) mapa == Porta -- && (pecaNaCoor (x+1,y+1) mapa == Bloco || pecaNaCoor (x+1,y+1) mapa == Caixa)
                                                              Trepar        -> if direcao == Este
                                                                               then (pecaNaCoor (x+1,y-1) mapa == Vazio || pecaNaCoor (x+1,y-1) mapa == Porta) && (pecaNaCoor (x+1,y) mapa == Bloco || pecaNaCoor (x+1,y) mapa == Caixa)
                                                                               else (pecaNaCoor (x-1,y-1) mapa == Vazio || pecaNaCoor (x-1,y-1) mapa == Porta) && (pecaNaCoor (x-1,y) mapa == Bloco || pecaNaCoor (x-1,y) mapa == Caixa)
                                                              InterageCaixa -> error "9" -- if direcao == Este 
                                                                                         -- then pecaNaCoor 

--Árvore que gera todos os movimentos possíveis de serem feitos.
data LITree a = SemCaminho | LI Jogo (LITree a) (LITree a) (LITree a) deriving Show

--Função que recebe um 'Jogo' e gera a árvore referida anteriormente.
geraTree :: Jogo -> Int -> LITree a
geraTree j@(Jogo mapa (Jogador (x,y) direcao box)) n | n >  1 && moveValido j AndarEsquerda && moveValido j AndarDireita && moveValido j Trepar = LI j (geraTree (moveJogador j AndarEsquerda) (n-1)) (geraTree (moveJogador j AndarDireita) (n-1)) (geraTree (moveJogador j Trepar) (n-1))
                                                     | n == 1 && moveValido j AndarEsquerda && moveValido j AndarDireita && moveValido j Trepar = LI j (LI (moveJogador j AndarEsquerda) SemCaminho SemCaminho SemCaminho) (LI (moveJogador j AndarDireita) SemCaminho SemCaminho SemCaminho) (LI (moveJogador j Trepar) SemCaminho SemCaminho SemCaminho)
                                                     | n >  1 && moveValido j AndarEsquerda && moveValido j AndarDireita = LI j (geraTree (moveJogador j AndarEsquerda) (n-1)) (geraTree (moveJogador j AndarDireita) (n-1)) SemCaminho
                                                     | n == 1 && moveValido j AndarEsquerda && moveValido j AndarDireita = LI j (LI (moveJogador j AndarEsquerda) SemCaminho SemCaminho SemCaminho) (LI (moveJogador j AndarDireita) SemCaminho SemCaminho SemCaminho) SemCaminho
                                                     | n >  1 && moveValido j AndarEsquerda && moveValido j Trepar       = LI j (geraTree (moveJogador j AndarEsquerda) (n-1)) SemCaminho (geraTree (moveJogador j Trepar) (n-1))
                                                     | n == 1 && moveValido j AndarEsquerda && moveValido j Trepar       = LI j (LI (moveJogador j AndarEsquerda) SemCaminho SemCaminho SemCaminho) SemCaminho (LI (moveJogador j Trepar) SemCaminho SemCaminho SemCaminho)
                                                     | n >  1 && moveValido j AndarDireita  && moveValido j Trepar       = LI j SemCaminho (geraTree (moveJogador j AndarDireita ) (n-1)) (geraTree (moveJogador j Trepar) (n-1))
                                                     | n == 1 && moveValido j AndarDireita  && moveValido j Trepar       = LI j SemCaminho (LI (moveJogador j AndarDireita) SemCaminho SemCaminho SemCaminho) (LI (moveJogador j Trepar) SemCaminho SemCaminho SemCaminho)
                                                     | n >  1 && moveValido j AndarEsquerda = LI j (geraTree (moveJogador j AndarEsquerda) (n-1)) SemCaminho SemCaminho
                                                     | n == 1 && moveValido j AndarEsquerda = LI j (LI (moveJogador j AndarEsquerda) SemCaminho SemCaminho SemCaminho) SemCaminho SemCaminho
                                                     | n >  1 && moveValido j AndarDireita  = LI j SemCaminho (geraTree (moveJogador j AndarDireita ) (n-1)) SemCaminho
                                                     | n == 1 && moveValido j AndarDireita  = LI j SemCaminho (LI (moveJogador j AndarDireita ) SemCaminho SemCaminho SemCaminho) SemCaminho
                                                     | n >  1 && moveValido j Trepar        = LI j SemCaminho SemCaminho (geraTree (moveJogador j Trepar) (n-1))
                                                     | n == 1 && moveValido j Trepar        = LI j SemCaminho SemCaminho (LI (moveJogador j Trepar) SemCaminho SemCaminho SemCaminho)
                                                     | otherwise = SemCaminho -- caso em que o boneco está bloqueado com caixa?


--Função que confirma a existência de solução num 'Mapa' com o número máximo de movimentos inseridos.
temSolucao :: Int -> Jogo -> Bool
temSolucao n j@(Jogo mapa (Jogador (x,y) _ _)) = temSolucao0 coor arv0
        where
        arv0 = geraTree j n
        coor = coorPorta mapa

--Função auxiliar da anterior: após a obtenção da árvore de todos os movimentos possíveis, indica se é possível a solução
temSolucao0 :: Coordenadas -> LITree a -> Bool
temSolucao0 _ SemCaminho = False
temSolucao0 c0@(x0,y0) (LI j@(Jogo _ (Jogador (x1,y1) _ _)) a b c) = (x0,y0) == (x1,y1) || temSolucao0 c0 a || temSolucao0 c0 b || temSolucao0 c0 c

--Função principal da Tarefa6 que indica a existência de solução: caso afirmativo, indica-nos o movimento.
resolveJogo :: Int -> Jogo -> Maybe [Movimento]
resolveJogo n j@(Jogo mapa (Jogador x _ _)) | door == x = Just []
                                            | temSolucao n j = Just way
                                            | otherwise = Nothing
      where
      door = coorPorta mapa
      arv0 = geraTree j n
      allmoves = concatMap inits (asSolucoes arv0)
      moveEgame = coordsJog allmoves j
      temporta  = isItDoor moveEgame door
      tdssolu   = dimRotas door temporta
      camicurtos= listaMoves tdssolu
      way = menosPassos camicurtos

--Cria lista de todos os movimentos possíveis com n movimentos.
asSolucoes :: LITree a -> [[Movimento]]
asSolucoes arv = case arv of
                      SemCaminho -> []
                      (LI _ SemCaminho SemCaminho SemCaminho) -> [[]]
                      (LI _ (LI a b c d) SemCaminho SemCaminho) -> map (plusMove AndarEsquerda) (asSolucoes (LI a b c d))
                      (LI _ SemCaminho (LI a b c d) SemCaminho) -> map (plusMove AndarDireita)  (asSolucoes (LI a b c d))
                      (LI _ SemCaminho SemCaminho (LI a b c d)) -> map (plusMove Trepar) (asSolucoes (LI a b c d))
                      (LI _ (LI a b c d) (LI a1 b1 c1 d1) SemCaminho) -> map (plusMove AndarEsquerda) (asSolucoes (LI a b c d)) ++ map (plusMove AndarDireita )(asSolucoes (LI a1 b1 c1 d1))
                      (LI _ (LI a b c d) SemCaminho (LI a1 b1 c1 d1)) -> map (plusMove AndarEsquerda) (asSolucoes (LI a b c d)) ++ map (plusMove Trepar) (asSolucoes (LI a1 b1 c1 d1))
                      (LI _ SemCaminho (LI a b c d) (LI a1 b1 c1 d1)) -> map (plusMove AndarDireita ) (asSolucoes (LI a b c d)) ++ map (plusMove Trepar) (asSolucoes (LI a1 b1 c1 d1))
                      (LI _ (LI a b c d) (LI a1 b1 c1 d1) (LI a2 b2 c2 d2)) -> map (plusMove AndarEsquerda)  (asSolucoes (LI a b c d)) ++ map (plusMove AndarDireita) (asSolucoes (LI a1 b1 c1 d1)) ++ map (plusMove Trepar) (asSolucoes (LI a2 b2 c2 d2))

--Função auxiliar para adicionar um movimento à lista.
plusMove ::Movimento -> [Movimento] -> [Movimento]
plusMove m x = m:x

--Função que recebe uma lista de lista de movimentos e um jogo, e dá uma lista de lista de movimentos com o jogo após ser executado o movimento.
coordsJog :: [[Movimento]] -> Jogo -> [[(Movimento,Jogo)]]
coordsJog [] _ = []
coordsJog ([]:t) j = coordsJog t j
coordsJog (x:xs) j = games x j : coordsJog xs j

--Função auxiliar da anterior.
games :: [Movimento] -> Jogo -> [(Movimento,Jogo)]
games [] _ = []
games (x:xs) j = (x,jogo) : games xs jogo
        where
        jogo = moveJogador j x

--Função que determina se o jogador alguma vez passa pela 'Porta'.
isItDoor :: [[(Movimento,Jogo)]] -> Coordenadas -> [[(Movimento,Jogo)]]
isItDoor [] _ = []
isItDoor (j:js) porta = if temPorta j porta then j : isItDoor js porta else isItDoor js porta

--Função auxiliar da anterior.
temPorta :: [(Movimento,Jogo)] -> Coordenadas -> Bool
temPorta [] _ = False
temPorta [(_,Jogo _ (Jogador c _ _))] porta = c == porta
temPorta ((_,Jogo _ (Jogador c _ _)):y) porta = c == porta || temPorta y porta

--Função que corta a rota no ponto em que o jogador atinge a 'Porta'. 
dimRota :: Coordenadas -> [(Movimento,Jogo)] -> [(Movimento,Jogo)]
dimRota _ []  = []
dimRota porta (j@(m,Jogo mapa (Jogador c d b)):t) | c == porta = [j]
                                                  | otherwise = j : dimRota porta t

--Função que aplica a função anterior para todos os movimentos possíveis que passam pela 'Porta'.
dimRotas :: Coordenadas -> [[(Movimento,Jogo)]] -> [[(Movimento, Jogo)]]
dimRotas _ [] = []
dimRotas c (x:xs) = dimRota c x : dimRotas c xs

--Função que retira somente os movimentos.
listaMove :: [(Movimento,Jogo)] -> [Movimento]
listaMove [] = []
listaMove ((a,b):c) = a : listaMove c

--Função que cria uma lista de lista de movimentos.
listaMoves :: [[(Movimento,Jogo)]] -> [[Movimento]]
listaMoves [] = []
listaMoves list = map listaMove list

--Função que nos fornece a menor rota.
menosPassos :: [[Movimento]] -> [Movimento]
menosPassos [] = []
menosPassos [x] = x
menosPassos (x:y:z) = if length x < length y then menosPassos (x:z) else menosPassos (y:z)


{- 

moveValido :: Jogo -> Movimento -> Bool
moveValido (Jogo mapa (Jogador (x,y) direcao box)) AndarEsquerda = pecaNaCoor (x-1,y) mapa == Vazio || pecaNaCoor (x-1,y) mapa == Porta 
moveValido (Jogo mapa (Jogador (x,y) direcao box)) AndarDireita  = pecaNaCoor (x+1,y) mapa == Vazio || pecaNaCoor (x+1,y) mapa == Porta 
moveValido (Jogo mapa (Jogador (x,y) Este    box)) Trepar        = (pecaNaCoor (x+1,y-1) mapa == Vazio || pecaNaCoor (x+1,y-1) mapa == Porta) && (pecaNaCoor (x+1,y) mapa == Bloco || pecaNaCoor (x+1,y) mapa == Caixa)
moveValido (Jogo mapa (Jogador (x,y) Oeste   box)) Trepar        = (pecaNaCoor (x-1,y-1) mapa == Vazio || pecaNaCoor (x-1,y-1) mapa == Porta) && (pecaNaCoor (x-1,y) mapa == Bloco || pecaNaCoor (x-1,y) mapa == Caixa)
moveValido (Jogo mapa (Jogador (x,y) Este  False)) InterageCaixa = pecaNaCoor (x+1,y) mapa == Caixa && pecaNaCoor (x,y-1) mapa == Vazio 
moveValido (Jogo mapa (Jogador (x,y) Oeste False)) InterageCaixa = pecaNaCoor (x-1,y) mapa == Caixa && pecaNaCoor (x,y-1) mapa == Vazio
moveValido (Jogo mapa (Jogador (x,y) Este   True)) InterageCaixa = pecaNaCoor (x+1,y) mapa == Vazio
moveValido (Jogo mapa (Jogador (x,y) Oeste  True)) InterageCaixa = pecaNaCoor (x-1,y) mapa == Vazio 


data LITree a = SemCaminho | LI Jogo (LITree a) (LITree a) (LITree a) (LITree a) deriving Show


geraTree :: Jogo -> Int -> LITree a

geraTree j@(Jogo mapa (Jogador (x,y) direcao box)) 1 | moveValido j AndarEsquerda && moveValido j AndarDireita  && moveValido j Trepar && moveValido j InterageCaixa = LI j (LI (moveJogador j AndarEsquerda) SemCaminho SemCaminho SemCaminho SemCaminho) (LI (moveJogador j AndarDireita) SemCaminho SemCaminho SemCaminho SemCaminho) (LI (moveJogador j Trepar) SemCaminho SemCaminho SemCaminho SemCaminho) (LI (moveJogador j InterageCaixa) SemCaminho SemCaminho SemCaminho SemCaminho) 
                                                     | moveValido j AndarEsquerda && moveValido j AndarDireita  && moveValido j Trepar        = LI j (LI (moveJogador j AndarEsquerda) SemCaminho SemCaminho SemCaminho SemCaminho) (LI (moveJogador j AndarDireita) SemCaminho SemCaminho SemCaminho SemCaminho) (LI (moveJogador j Trepar) SemCaminho SemCaminho SemCaminho SemCaminho) SemCaminho
                                                     | moveValido j AndarEsquerda && moveValido j AndarDireita  && moveValido j InterageCaixa = LI j (LI (moveJogador j AndarEsquerda) SemCaminho SemCaminho SemCaminho SemCaminho) (LI (moveJogador j AndarDireita) SemCaminho SemCaminho SemCaminho SemCaminho) SemCaminho (LI (moveJogador j InterageCaixa) SemCaminho SemCaminho SemCaminho SemCaminho)
                                                     | moveValido j AndarEsquerda && moveValido j InterageCaixa && moveValido j Trepar        = LI j (LI (moveJogador j AndarEsquerda) SemCaminho SemCaminho SemCaminho SemCaminho) SemCaminho (LI (moveJogador j Trepar) SemCaminho SemCaminho SemCaminho SemCaminho) (LI (moveJogador j InterageCaixa) SemCaminho SemCaminho SemCaminho SemCaminho)
                                                     | moveValido j AndarEsquerda && moveValido j AndarDireita  = LI j (LI (moveJogador j AndarEsquerda) SemCaminho SemCaminho SemCaminho SemCaminho) (LI (moveJogador j AndarDireita) SemCaminho SemCaminho SemCaminho SemCaminho) SemCaminho SemCaminho
                                                     | moveValido j AndarEsquerda && moveValido j Trepar        = LI j (LI (moveJogador j AndarEsquerda) SemCaminho SemCaminho SemCaminho SemCaminho) SemCaminho (LI (moveJogador j Trepar) SemCaminho SemCaminho SemCaminho SemCaminho) SemCaminho
                                                     | moveValido j AndarEsquerda && moveValido j InterageCaixa = LI j (LI (moveJogador j AndarEsquerda) SemCaminho SemCaminho SemCaminho SemCaminho) SemCaminho SemCaminho (LI (moveJogador j InterageCaixa) SemCaminho SemCaminho SemCaminho SemCaminho)
                                                     | moveValido j AndarDireita  && moveValido j Trepar        = LI j SemCaminho (LI (moveJogador j AndarDireita) SemCaminho SemCaminho SemCaminho SemCaminho) (LI (moveJogador j Trepar) SemCaminho SemCaminho SemCaminho SemCaminho) SemCaminho
                                                     | moveValido j AndarDireita  && moveValido j InterageCaixa = LI j SemCaminho (LI (moveJogador j AndarDireita) SemCaminho SemCaminho SemCaminho SemCaminho) SemCaminho (LI (moveJogador j InterageCaixa) SemCaminho SemCaminho SemCaminho SemCaminho)
                                                     | moveValido j InterageCaixa && moveValido j Trepar        = LI j SemCaminho SemCaminho (LI (moveJogador j Trepar) SemCaminho SemCaminho SemCaminho SemCaminho) (LI (moveJogador j InterageCaixa) SemCaminho SemCaminho SemCaminho SemCaminho)
                                                     | moveValido j AndarEsquerda = LI j (LI (moveJogador j AndarEsquerda) SemCaminho SemCaminho SemCaminho SemCaminho) SemCaminho SemCaminho SemCaminho
                                                     | moveValido j AndarDireita  = LI j SemCaminho (LI (moveJogador j AndarDireita ) SemCaminho SemCaminho SemCaminho SemCaminho) SemCaminho SemCaminho
                                                     | moveValido j Trepar        = LI j SemCaminho SemCaminho (LI (moveJogador j Trepar) SemCaminho SemCaminho SemCaminho SemCaminho) SemCaminho
                                                     | moveValido j InterageCaixa = LI j SemCaminho SemCaminho SemCaminho (LI (moveJogador j InterageCaixa) SemCaminho SemCaminho SemCaminho SemCaminho)
                                                     | otherwise = SemCaminho
                                           

geraTree j@(Jogo mapa (Jogador (x,y) direcao box)) n | moveValido j AndarEsquerda && moveValido j AndarDireita && moveValido j Trepar && moveValido j InterageCaixa = LI j (geraTree (moveJogador j AndarEsquerda) (n-1)) (geraTree (moveJogador j AndarDireita) (n-1)) (geraTree (moveJogador j Trepar) (n-1)) (geraTree (moveJogador j InterageCaixa) (n-1))
                                                     | moveValido j AndarEsquerda && moveValido j AndarDireita && moveValido j Trepar        = LI j (geraTree (moveJogador j AndarEsquerda) (n-1)) (geraTree (moveJogador j AndarDireita) (n-1)) (geraTree (moveJogador j Trepar) (n-1)) SemCaminho
                                                     | moveValido j AndarEsquerda && moveValido j AndarDireita && moveValido j InterageCaixa = LI j (geraTree (moveJogador j AndarEsquerda) (n-1)) (geraTree (moveJogador j AndarDireita) (n-1)) SemCaminho (geraTree (moveJogador j InterageCaixa) (n-1))
                                                     | moveValido j AndarEsquerda && moveValido j InterageCaixa && moveValido j Trepar       = LI j (geraTree (moveJogador j AndarEsquerda) (n-1)) SemCaminho (geraTree (moveJogador j Trepar) (n-1)) (geraTree (moveJogador j InterageCaixa) (n-1))
                                                     | moveValido j AndarEsquerda && moveValido j AndarDireita  = LI j (geraTree (moveJogador j AndarEsquerda) (n-1)) (geraTree (moveJogador j AndarDireita) (n-1)) SemCaminho SemCaminho
                                                     | moveValido j AndarEsquerda && moveValido j Trepar        = LI j (geraTree (moveJogador j AndarEsquerda) (n-1)) SemCaminho (geraTree (moveJogador j Trepar) (n-1)) SemCaminho
                                                     | moveValido j AndarEsquerda && moveValido j InterageCaixa = LI j (geraTree (moveJogador j AndarEsquerda) (n-1)) SemCaminho SemCaminho (geraTree (moveJogador j InterageCaixa) (n-1)) 
                                                     | moveValido j AndarDireita  && moveValido j Trepar        = LI j SemCaminho (geraTree (moveJogador j AndarDireita) (n-1)) (geraTree (moveJogador j Trepar) (n-1)) SemCaminho
                                                     | moveValido j AndarDireita  && moveValido j InterageCaixa = LI j SemCaminho (geraTree (moveJogador j AndarDireita) (n-1)) SemCaminho (geraTree (moveJogador j InterageCaixa) (n-1))                                                  
                                                     | moveValido j AndarDireita  = LI j SemCaminho (geraTree (moveJogador j AndarDireita) (n-1)) SemCaminho SemCaminho 
                                                     | moveValido j Trepar        = LI j SemCaminho SemCaminho (geraTree (moveJogador j Trepar) (n-1)) SemCaminho
                                                     | moveValido j InterageCaixa = LI j SemCaminho SemCaminho SemCaminho (geraTree (moveJogador j InterageCaixa) (n-1))
                                                     | otherwise = SemCaminho


temSolucao :: Int -> Jogo -> Bool
temSolucao n j@(Jogo mapa (Jogador (x,y) _ _)) = temSolucao0 coor arv0
        where
        arv0 = geraTree j n
        coor = xyPorta mapa


temSolucao0 :: Coordenadas -> LITree a -> Bool
temSolucao0 _ SemCaminho = False
temSolucao0 c0@(x0,y0) (LI j@(Jogo _ (Jogador (x1,y1) _ _)) a b c d) = (x0,y0) == (x1,y1) || temSolucao0 c0 a || temSolucao0 c0 b || temSolucao0 c0 c || temSolucao0 c0 d



asSolucoes :: LITree a -> [[Movimento]]
asSolucoes arv = case arv of
                      SemCaminho -> []
                      (LI _ SemCaminho   SemCaminho SemCaminho SemCaminho) -> [[]]
                      (LI _ (LI e a b c d) SemCaminho SemCaminho SemCaminho) -> map (plusMove AndarEsquerda) (asSolucoes (LI e a b c d))
                      (LI _ SemCaminho (LI e a b c d) SemCaminho SemCaminho) -> map (plusMove AndarDireita ) (asSolucoes (LI e a b c d))
                      (LI _ SemCaminho SemCaminho (LI e a b c d) SemCaminho) -> map (plusMove Trepar) (asSolucoes (LI e a b c d))
                      (LI _ SemCaminho SemCaminho SemCaminho (LI e a b c d)) -> map (plusMove InterageCaixa) (asSolucoes (LI e a b c d))
                      (LI _ (LI e a b c d) (LI e1 a1 b1 c1 d1) SemCaminho SemCaminho) -> map (plusMove AndarEsquerda) (asSolucoes (LI e a b c d)) ++ map (plusMove AndarDireita )(asSolucoes (LI e1 a1 b1 c1 d1))
                      (LI _ (LI e a b c d) SemCaminho (LI e1 a1 b1 c1 d1) SemCaminho) -> map (plusMove AndarEsquerda) (asSolucoes (LI e a b c d)) ++ map (plusMove Trepar) (asSolucoes (LI e1 a1 b1 c1 d1))
                      (LI _ (LI e a b c d) SemCaminho SemCaminho (LI e1 a1 b1 c1 d1)) -> map (plusMove AndarEsquerda) (asSolucoes (LI e a b c d)) ++ map (plusMove InterageCaixa) (asSolucoes (LI e1 a1 b1 c1 d1))
                      (LI _ SemCaminho (LI e a b c d) (LI e1 a1 b1 c1 d1) SemCaminho) -> map (plusMove AndarDireita ) (asSolucoes (LI e a b c d)) ++ map (plusMove Trepar) (asSolucoes (LI e1 a1 b1 c1 d1))
                      (LI _ SemCaminho (LI e a b c d) SemCaminho (LI e1 a1 b1 c1 d1)) -> map (plusMove AndarDireita ) (asSolucoes (LI e a b c d)) ++ map (plusMove InterageCaixa) (asSolucoes (LI e1 a1 b1 c1 d1))
                      (LI _ (LI e a b c d) (LI e1 a1 b1 c1 d1) (LI e2 a2 b2 c2 d2) SemCaminho) -> map (plusMove AndarEsquerda) (asSolucoes (LI e a b c d)) ++ map (plusMove AndarDireita) (asSolucoes (LI e1 a1 b1 c1 d1)) ++ map (plusMove Trepar) (asSolucoes (LI e2 a2 b2 c2 d2))
                      (LI _ (LI e a b c d) SemCaminho (LI e1 a1 b1 c1 d1) (LI e2 a2 b2 c2 d2)) -> map (plusMove AndarEsquerda) (asSolucoes (LI e a b c d)) ++ map (plusMove Trepar) (asSolucoes (LI e1 a1 b1 c1 d1)) ++ map (plusMove InterageCaixa) (asSolucoes (LI e2 a2 b2 c2 d2))
                      (LI _ (LI e a b c d) (LI e1 a1 b1 c1 d1) SemCaminho (LI e2 a2 b2 c2 d2)) -> map (plusMove AndarEsquerda) (asSolucoes (LI e a b c d)) ++ map (plusMove AndarDireita) (asSolucoes (LI e1 a1 b1 c1 d1)) ++ map (plusMove InterageCaixa) (asSolucoes (LI e2 a2 b2 c2 d2))
                      (LI _ SemCaminho (LI e a b c d) (LI e1 a1 b1 c1 d1) (LI e2 a2 b2 c2 d2)) -> map (plusMove AndarDireita ) (asSolucoes (LI e a b c d)) ++ map (plusMove Trepar) (asSolucoes (LI e1 a1 b1 c1 d1)) ++ map (plusMove InterageCaixa) (asSolucoes (LI e2 a2 b2 c2 d2))
                      (LI _ (LI e a b c d) (LI e1 a1 b1 c1 d1) (LI e2 a2 b2 c2 d2) (LI e3 a3 b3 c3 d3)) -> map (plusMove AndarEsquerda) (asSolucoes (LI e a b c d)) ++ map (plusMove AndarDireita) (asSolucoes (LI e1 a1 b1 c1 d1)) ++ map (plusMove Trepar) (asSolucoes (LI e2 a2 b2 c2 d2)) ++ map (plusMove InterageCaixa) (asSolucoes (LI e3 a3 b3 c3 d3))
-}

mapa0 :: Mapa
mapa0 = [[Vazio,Porta],
         [Bloco,Bloco]]

mapa1 :: Mapa
mapa1 = [[Vazio,Vazio,Vazio],
         [Vazio,Vazio,Porta],
         [Bloco,Bloco,Bloco]]

mapa2 :: Mapa
mapa2 = [[Vazio,Vazio,Vazio,Porta],
         [Bloco,Vazio,Bloco,Bloco],
         [Bloco,Bloco,Vazio,Vazio]]

mapa3 :: Mapa
mapa3 = [[Vazio,Vazio,Vazio,Vazio,Porta],
         [Vazio,Vazio,Vazio,Bloco,Bloco],
         [Bloco,Bloco,Bloco,Bloco,Bloco]]

mapa4 :: Mapa 
mapa4 = [[Vazio,Vazio,Vazio,Porta],
         [Vazio,Vazio,Vazio,Bloco],
         [Caixa,Vazio,Vazio,Bloco],
         [Bloco,Bloco,Bloco,Bloco]]

mapa5 :: Mapa
mapa5 = [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
         [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
         [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
         [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
         [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Porta],
         [Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco],
         [Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio]]

mapa6 :: Mapa
mapa6 = [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
         [Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio],
         [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
         [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
         [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
         [Vazio,Porta,Vazio,Vazio,Vazio,Bloco,Bloco],
         [Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio]]


jogo0 :: Jogo
jogo0 = Jogo mapa0 (Jogador (0,0) Oeste False)

jogo1 :: Jogo
jogo1 = Jogo mapa1 (Jogador (0,1) Este False)

jogo2 :: Jogo
jogo2 = Jogo mapa2 (Jogador (0,0) Este False)

jogo3 :: Jogo
jogo3 = Jogo mapa3 (Jogador (0,1) Este False)

jogo4 :: Jogo
jogo4 = Jogo mapa4 (Jogador (1,2) Oeste False)

jogo5 :: Jogo
jogo5 = Jogo mapa5 (Jogador (0,5) Este False)

jogo6 :: Jogo
jogo6 = Jogo mapa6 (Jogador (0,0) Este False)


