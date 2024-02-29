module Tarefa3_2021li1g085 where

import LI12122

instance Show Jogo where
    show = showtudo


{- | Quando é 'Jogador', a função retira 1 ao eixo x à medida que o 'Jogador' chega ao fim de cada lista, mudando de linha até encontrar a sua posição. Quando esta encontra a sua posição, recorre-se a 'showjogador' para mostrar o seu sentido. E, em caso de ser uma 'Peca', a função utiliza 'showPeca'.

@
showjogo :: Jogo -> String
showjogo (Jogo [[]] (Jogador (x,y) _ _)) = ""
showjogo (Jogo ([]:ts) (Jogador (x,y) direcao temCaixa)) = "\n" ++ showjogo (Jogo ts (Jogador (x,y-1) direcao temCaixa))
showjogo (Jogo ((peca:xs):ts) (Jogador (x,y) direcao temCaixa)) | x == 0 && y == 0 = (showjogador direcao) ++ showjogo (Jogo (xs:ts) (Jogador (-1,-1) direcao temCaixa))
                                                                | y == 0 = showpeca peca ++ showjogo (Jogo (xs:ts) (Jogador (x-1,y) direcao temCaixa))
                                                                | otherwise = showpeca peca ++ showjogo (Jogo (xs:ts) (Jogador (x,y) direcao temCaixa))
@

-}

showjogo :: Jogo     -- ^ argumento
           -> String -- ^ 'Ḿapa' em 'String'
showjogo (Jogo [[]] (Jogador (x,y) _ _)) = ""
showjogo (Jogo [] _) = " "
showjogo (Jogo ([]:ts) (Jogador (x,y) direcao temCaixa)) = "\n" ++ showjogo (Jogo ts (Jogador (x,y-1) direcao temCaixa))
showjogo (Jogo ((peca:xs):ts) (Jogador (x,y) direcao temCaixa)) | x == 0 && y == 0 = showjogador direcao ++ showjogo (Jogo (xs:ts) (Jogador (-1,-1) direcao temCaixa))
                                                                | y == 0 = showpeca peca ++ showjogo (Jogo (xs:ts) (Jogador (x-1,y) direcao temCaixa))
                                                                | otherwise = showpeca peca ++ showjogo (Jogo (xs:ts) (Jogador (x,y) direcao temCaixa))


{- | Esta função faz correspondência de cada 'String' com a respetiva 'Peca'.

@
showpeca :: Peca -> String
showpeca x = case x of
                Bloco -> "X"
                Caixa -> "C"
                Porta -> "P"
                Vazio -> " "
@
-}

showpeca :: Peca      -- ^ 4 casos possíveis
            -> String -- ^ respetiva letra
showpeca x = case x of
                Bloco -> "X"
                Caixa -> "C"
                Porta -> "P"
                Vazio -> " "


{- | Esta função mostra a direção do 'Jogador'.

@
showjogador :: Direcao -> String 
showjogador x = case x of 
                     Este  -> ">"
                     Oeste -> "<"
@
-}

showjogador :: Direcao   -- ^ 2 casos possíveis
               -> String -- ^ respetivo sentido
showjogador x = case x of
                     Este  -> ">"
                     Oeste -> "<"


{- | Esta função pega no 'Mapa' já em 'String' e devolve-a com a devida caixa.

@
showgame :: String -> Coordenadas -> String
showgame [] _ = []
showgame (z:zs) (x,y) | y == 0 && x == 0 = 'C' : zs 
                      | y == 0 && x >  0 = z : showgame zs (x-1,y)
                      | y >= 1 && z == '\n' = z : showgame zs (x,y-1)
                      | otherwise = z : showgame zs (x,y)
@
-}

showgame :: String         -- ^ 'Mapa' convertido
            -> Coordenadas -- ^ 'Coordenadas' da 'Caixa'
            -> String      -- ^ 'Mapa' com 'Caixa'

showgame [] _ = []
showgame (z:zs) (x,y) | y == 0 && x == 0 = 'C' : zs
                      | y == 0 && x >  0 = z : showgame zs (x-1,y)
                      | y >= 1 && z == '\n' = z : showgame zs (x,y-1)
                      | otherwise = z : showgame zs (x,y)



{- | Esta função tem dois casos: um para quando o jogador tem caixa, e outro quando este não tem.

@
showtudo :: Jogo -> String 
showtudo (Jogo [[]] (Jogador _ _ _)) = []
showtudo j@(Jogo mapa (Jogador (x,y) direcao box)) = case box of 
                                                          False -> showjogo j
                                                          True  -> showgame (showjogo j) (x,y-1)
@
-}

showtudo :: Jogo      -- ^ argumento
            -> String -- ^ 'Mapa' em 'String'

showtudo (Jogo [[]] Jogador {}) = []
showtudo j@(Jogo mapa (Jogador (x,y) direcao box)) = if box then showgame (showjogo j) (x,y-1) else showjogo j
