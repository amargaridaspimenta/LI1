module Fixtures where

import LI12122

m1 :: [(Peca, Coordenadas)]
m1 =
  [ (Porta, (0, 3)),
    (Bloco, (0, 4)),
    (Bloco, (1, 4)),
    (Bloco, (2, 4)),
    (Bloco, (3, 4)),
    (Bloco, (4, 4)),
    (Caixa, (4, 3)),
    (Bloco, (5, 4)),
    (Bloco, (6, 4)),
    (Bloco, (6, 3)),
    (Bloco, (6, 2)),
    (Bloco, (6, 1))
  ]

m1r :: Mapa
m1r =
 [[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
        [Bloco, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Vazio, Bloco],
        [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
        [Porta, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Caixa, Vazio, Vazio, Bloco],
        [Bloco, Caixa, Bloco, Bloco, Vazio, Caixa, Vazio, Bloco, Bloco, Vazio, Bloco],
        [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]

m5r :: Mapa
m5r =
  [ [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Porta, Vazio],
    [Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Bloco, Vazio, Bloco, Vazio, Bloco, Bloco, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Bloco, Vazio, Bloco, Vazio, Vazio, Vazio]
  ]


m1e1 :: Jogo
m1e1 = Jogo m1r (Jogador (10, 2) Oeste False)

m1e2 :: Jogo
m1e2 = Jogo m1r (Jogador (9, 4) Oeste False)

m1e3 :: Jogo
m1e3 = Jogo m1r (Jogador (1, 3) Este  False)

m1e4 :: Jogo
m1e4 = Jogo m1r (Jogador (1, 2) Este  True )

m1e5 :: Jogo
m1e5 = Jogo m5r (Jogador (4, 2) Este  False)


m2 :: [(Peca,Coordenadas)]
m2 = 
  [ (Porta, (0, 3)),
    (Bloco, (0, 4)),
    (Bloco, (1, 4)),
    (Bloco, (2, 4)),
    (Bloco, (3, 4)),
    (Bloco, (4, 4)),
    (Caixa, (4, 4)),
    (Bloco, (5, 4)),
    (Bloco, (6, 4)),
    (Bloco, (6, 3)),
    (Bloco, (6, 2)),
    (Bloco, (6, 1))]

m3 :: [(Peca,Coordenadas)]
m3 = 
  [ (Porta, (0, 3)),
    (Bloco, (0, 4)),
    (Bloco, (1, 4)),
    (Bloco, (2, 4)),
    (Bloco, (3, 4)),
    (Bloco, (4, 4)),
    (Caixa, (4, 2)),
    (Bloco, (5, 4)),
    (Bloco, (6, 4)),
    (Bloco, (6, 3)),
    (Bloco, (6, 2)),
    (Bloco, (6, 1))]


m4 :: [(Peca,Coordenadas)]
m4 =
  [ (Porta, (0, 3)),
    (Bloco, (0, 4)),
    (Bloco, (1, 4)),
    (Bloco, (2, 4)),
    (Bloco, (3, 4)),
    (Bloco, (4, 4)),
    (Caixa, (4, 3)),
    (Bloco, (5, 4)),
    (Bloco, (6, 4)),
    (Bloco, (6, 3)),
    (Bloco, (6, 2)),
    (Bloco, (8, 1))]

m5 :: [(Peca,Coordenadas)]
m5 =
  [ (Bloco,(0,0)),
    (Bloco,(1,1)),
    (Bloco,(2,2)),
    (Bloco,(3,3)),
    (Bloco,(4,3)),
    (Bloco,(5,3)),
    (Bloco,(5,4)),
    (Bloco,(6,4)),
    (Bloco,(7,3)),
    (Bloco,(8,4)),
    (Bloco,(9,3)),
    (Caixa,(4,2)),
    (Porta,(12,2)),
    (Bloco,(10,4)),
    (Bloco,(11,3)),
    (Bloco,(12,3)),
    (Bloco,(13,3))]


    
m5i :: String
m5i = "X             \n X            \n  X >       P \n   XXX X X XXX\n     XX X X   "


m1' :: [(Peca, Coordenadas)]
m1' =
  [(Porta, (0, 2)), 
  (Bloco, (0, 3)),
   (Bloco, (1, 3)), 
  (Bloco, (2, 4)), 
  (Bloco, (3, 4)), 
  (Bloco, (4, 4)), 
  (Caixa, (4, 3)), 
  (Bloco, (5, 2)), 
  (Bloco, (5, 3)), 
  (Bloco, (5, 4)), 
  (Vazio, (2, 3)), 
  (Bloco, (2, 4))]

m1r' :: Mapa
m1r' =
  [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
  [Porta, Vazio, Vazio, Vazio, Vazio, Bloco],
  [Bloco, Bloco, Vazio, Vazio, Caixa, Bloco],
  [Vazio, Bloco, Bloco, Bloco, Bloco, Bloco]]

m1e1' :: Jogo
m1e1' = Jogo m1r' (Jogador (5, 2) Oeste False)

m1e2' :: Jogo
m1e2' = Jogo m1r' (Jogador (3, 1) Este False)
