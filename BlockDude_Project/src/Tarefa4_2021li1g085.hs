module Tarefa4_2021li1g085 where

import Tarefa2_2021li1g085 hiding (desconstroi, desconstroiMapa)
import LI12122
    ( Coordenadas,
      Peca(Caixa, Bloco, Vazio, Porta ),
      Direcao(..),
      Jogador(..),
      Movimento(..),
      Jogo(..), Mapa )

{- | 
A função @moveJogador@ recebe um Jogo e um Movimento retribuindo um novo Jogo, sendo este dependente do movimento aplicado.
-}

moveJogador :: Jogo -> Movimento -> Jogo
moveJogador (Jogo mapa (Jogador (x,y) d c)) AndarDireita 
  | x==xmax = Jogo mapa (Jogador (x,y) Este c)
  | c==False && elem (Vazio,(x+1,y)) l || c==False && elem (Porta,(x+1,y)) l = Jogo mapa (Jogador (movimentodireita l ((x,y),d)) Este False)
  | c==False && notElem (Vazio,(x+1,y)) l || c==False && notElem (Porta,(x+1,y)) l = Jogo mapa (Jogador (x,y) Este False)
  | c==True && elem (Vazio,(x+1,y)) l && elem (Vazio,(x+1,y-1)) l= Jogo mapa (Jogador (podeavancardireita l ((x,y),d)) Este True)
  | c==True && notElem (Vazio,(x+1,y)) l || c==True && notElem (Vazio,(x+1,y-1)) l = Jogo mapa (Jogador (x,y) Este True)
 where
     l = desconstroiMapa (inverte mapa)
     xmax = maxX l

moveJogador (Jogo mapa (Jogador (x,y) d c)) AndarEsquerda 
  | x==0  = Jogo mapa (Jogador (x,y) Oeste c)
  | c==False && elem (Vazio,(x-1,y)) l || c==False && elem (Porta,(x-1,y)) l  = Jogo mapa (Jogador (movimentoesquerda l ((x,y),d)) Oeste  False)
  | c==False && notElem (Vazio,(x-1,y)) l || c==False && notElem (Porta,(x+1,y)) l = Jogo mapa (Jogador (x,y) Oeste False)
  | c==True  && elem (Vazio,(x-1,y)) l && elem (Vazio,(x-1,y-1)) l= Jogo mapa (Jogador (podeavancaresquerda l ((x,y),d)) Oeste True)
  | c==True && notElem (Vazio,(x-1,y)) l || c==True && notElem (Vazio,(x-1,y-1)) l = Jogo mapa (Jogador (x,y) Oeste True)
 where
     l = desconstroiMapa (inverte mapa)

moveJogador (Jogo mapa (Jogador (x,y) d c)) Trepar 
  | (d==Este && x==xmax) || (d==Oeste && y==0)  = Jogo mapa (Jogador (x,y) d c)
  | y==0 = Jogo mapa (Jogador (x,y) d c)
  | c==False && d==Este && elem (Vazio,(x+1,y-1)) l && (elem (Caixa,(x+1,y)) l || elem (Bloco,(x+1,y)) l) = Jogo mapa (Jogador (treparsemcaixa l ((x,y),d)) Este False)
  | c==False && d==Oeste && elem (Vazio,(x-1,y-1)) l && (elem (Caixa,(x-1,y)) l || elem (Bloco,(x-1,y)) l) = Jogo mapa (Jogador (treparsemcaixa l ((x,y),d)) Oeste False)
  | c==True && d==Este && elem (Vazio,(x+1,y-1)) l && elem (Vazio,(x+1,y-2)) l && (elem (Caixa,(x+1,y)) l || elem (Bloco,(x+1,y)) l) = Jogo mapa (Jogador (treparcomcaixa l ((x,y),d)) Este True)
  | c==True && d==Oeste && elem (Vazio,(x-1,y-1)) l && elem (Vazio,(x-1,y-2)) l && (elem (Caixa,(x-1,y)) l || elem (Bloco,(x-1,y)) l) = Jogo mapa (Jogador (treparcomcaixa l ((x,y),d)) Oeste True)
  | otherwise = Jogo mapa (Jogador (x,y) d c)
 where
     l = desconstroiMapa (inverte mapa)
     xmax = maxX l

moveJogador j@(Jogo mapa (Jogador (x,y) d c)) InterageCaixa
  | y==0 = Jogo mapa (Jogador (x,y) d c)
  | x==0 && d==Oeste || x==xmax && d==Este = Jogo mapa (Jogador (x,y) d c)
  | c==False && d==Oeste && elem (Vazio,(x-1,y-1)) l && elem (Caixa,(x-1,y)) l && notElem (Bloco,(x,y-1)) l = Jogo mapasemcaixa (Jogador (x,y) Oeste True)
  | c==False && d==Este && elem (Vazio,(x+1,y-1)) l && elem (Caixa,(x+1,y)) l && notElem (Bloco,(x,y-1)) l = Jogo mapasemcaixa (Jogador (x,y) Este True)
  | c==True && d==Oeste && elem (Vazio,(x-1,y)) l && elem (Vazio,(x-1,y-1)) l && (elem (Caixa,(x-1,y+1)) l || elem (Bloco,(x-1,y+1)) l) = Jogo mapacomcaixa (Jogador (x,y) Oeste False)
  | c==True && d==Oeste && notElem (Vazio,(x-1,y)) l && elem (Vazio,(x-1,y-1)) l = Jogo mapacomcaixa (Jogador (x,y) Oeste False)
  | c==True && d==Este && elem (Vazio,(x+1,y)) l && elem (Vazio,(x+1,y-1)) l && (elem (Caixa,(x+1,y+1)) l || elem (Bloco,(x+1,y+1)) l) = Jogo mapacomcaixa (Jogador (x,y) Este False)
  | c==True && d==Este && notElem (Vazio,(x+1,y)) l && elem (Vazio,(x-1,y-1)) l = Jogo mapacomcaixa (Jogador (x,y) Este False)
  | otherwise = j
  where
     l = desconstroiMapa (inverte mapa)
     xmax = maxX l
     mapasemcaixa = jogapanhacaixa mapa ((x,y),d)
     mapacomcaixa = jogpousacaixa mapa ((x,y),d)

{- |
A função @correrMovimentos@ receber um Jogo e uma lista de Movimentos, retribuindo assim um novo Jogo, resultante da consecutiva aplicação de cada um dos 
movimentos pertencentes à lista.
-}

correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos (Jogo l (Jogador (x,y) d c)) [] = Jogo l (Jogador (x,y) d c)
correrMovimentos (Jogo l (Jogador (x,y) d c)) (h:t) = correrMovimentos (moveJogador (Jogo l (Jogador (x,y) d c)) h) t

{- | 
A função @posdirJog@ recebe um Jogador e retribui as suas coordenadas e direção. 

Exemplo: Se o jogador estiver na posição (0,3) e virado para Este e a carregar uma caixa (Jogador (0,3) Este True), através da aplicação da função 
recebemos ((0,3),Este).
-}
posdirJog :: Jogador -> (Coordenadas,Direcao)
posdirJog (Jogador (x,y) d b) = ((x,y),d)


{- |
Para que o jogador execute um movimento é necessário saber se esse movimento é possível e, para isso, iremos recorrer à função @desconstroiMapa@, já utilizada 
na _Tarefa2_, que a partir de um mapa, devolve a sua respetiva lista de peças e coordenadas, incluindo as peças Vazio.
Utilizamos esta função pois facilita a procura e verificação de peças. 
-}

desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa [] = []
desconstroiMapa ([]:t) = desconstroiMapa t
desconstroiMapa l  = desconstroi l (0,y-1)
    where
        y = length l

desconstroi :: Mapa -> (Int,Int) -> [(Peca,Coordenadas)]
desconstroi [] _ = []
desconstroi ([]:t) (n1,n2)  = desconstroi t (0,n2-1)
desconstroi ((x:xs):t) (n1,n2) = (x,(n1,n2)) : desconstroi (xs:t) (n1+1,n2)


{- |
__AndarEsquerda__

A função @movimentoesquerda@ verifica, a partir de uma lista de peças e coordenadas e das coordenadas do jogador, se este pode ou não executar o movimento 
_AndarEsquerda_.

Considerando _(x,y)_ as coordenadas do jogador:

Este movimento só é possível caso no espaço imediatamente à esquerda do jogador (x-1,y), exista uma peça Vazio, podendo ele, neste movimento, apenas deslocar-se
no eixo das abcissas para a esquerda (caso se verifique a existência de um bloco ou caixa no espaço à sua esquerda e abaixo uma unidade, ou seja, em (x-1,y+1)) 
- 3ª guarda da função - ou sofrer uma situação de queda (caso não se verifique a existência de um bloco ou caixa no espaço à sua esquerda e abaixo uma unidade, 
ou seja, em (x-1,y+1)) - 5ª guarda da função (a 2ª guarda representa um caso de paragem para a situação de queda).
Para a situação de queda recorremos à recursividade da função @movimentoesquerda@.
Caso não se verifiquem estas condições, o jogador não altera a sua posição.
-}

movimentoesquerda :: [(Peca,Coordenadas)] -> (Coordenadas,Direcao) -> Coordenadas
movimentoesquerda [] _ = (0,0)
movimentoesquerda ((p,(x1,y1)):t) ((x,y),d)
  | x==0 = (x,y)
  | y==ymax = (x-1,y-1)
  | (elem (Caixa,(x-1,y+1)) ((p,(x1,y1)):t) || elem (Bloco,(x-1,y+1)) ((p,(x1,y1)):t)) && (elem (Porta,(x-1,y)) ((p,(x1,y1)):t) || elem (Vazio,(x-1,y)) ((p,(x1,y1)):t)) = (x-1,y)
  | (elem (Caixa,(x-1,y+1)) ((p,(x1,y1)):t) || elem (Bloco,(x-1,y+1)) ((p,(x1,y1)):t)) && (notElem (Porta,(x-1,y)) ((p,(x1,y1)):t) || notElem (Vazio,(x-1,y)) ((p,(x1,y1)):t)) = (x,y)
  | notElem (Caixa,(x-1,y+1)) ((p,(x1,y1)):t) || notElem (Bloco,(x-1,y+1)) ((p,(x1,y1)):t) && elem (Vazio,(x-1,y)) ((p,(x1,y1)):t) = andaesquerda
  | otherwise = (x,y)
   where
       ymax         = maxY ((p,(x,y)):t)
       andaesquerda = movimentoesquerda ((p,(x1,y1)):t) ((x,y+1),d)


{- |
__AndarDireita__

A função @movimentodireita@ verifica, a partir de uma lista de peças e coordenadas e das coordenadas do jogador, se este pode ou não executar o movimento 
_AndarDireita_.

Considerando _(x,y)_ as coordenadas do jogador:

Este movimento só é possível caso no espaço imediatamente à direita do jogador (x+1,y), exista uma peça Vazio, podendo ele, neste movimento, apenas deslocar-se 
uma unidade no eixo das abcissas para a direita (caso se verifique a existência de um bloco ou caixa no espaço à sua direita e abaixo uma unidade, ou seja, em 
(x+1,y+1)) - 3ª guarda da função - ou sofrer uma situação de queda (caso não se verifique a existência de um bloco ou caixa no espaço à sua direita e abaixo 
uma unidade, ou seja, em (x+1,y+1)) - 5ª guarda da função (a 2ª guarda representa um caso de paragem para a situação de queda).
Para a situação de queda recorremos à recursividade da função @movimentodireita@.
Caso não se verifiquem estas condições, o jogador não altera a sua posição.
-}

movimentodireita :: [(Peca,Coordenadas)] -> (Coordenadas,Direcao) -> Coordenadas
movimentodireita [] _ = (0,0)
movimentodireita ((p,(x1,y1)):t) ((x,y),d)
  | x==xmax = (x,y)
  | y==ymax = (x+1,y-1)
  | (elem (Caixa,(x+1,y+1)) ((p,(x1,y1)):t) || elem (Bloco,(x+1,y+1)) ((p,(x1,y1)):t)) && (elem (Porta,(x+1,y)) ((p,(x1,y1)):t) || elem (Vazio,(x+1,y)) ((p,(x1,y1)):t)) = (x+1,y)
  | (elem (Caixa,(x+1,y+1)) ((p,(x1,y1)):t) || elem (Bloco,(x+1,y+1)) ((p,(x1,y1)):t)) && (notElem (Porta,(x+1,y)) ((p,(x1,y1)):t) || notElem (Vazio,(x+1,y)) ((p,(x1,y1)):t)) = (x,y)
  | notElem (Caixa,(x+1,y+1)) ((p,(x1,y1)):t) || notElem (Bloco,(x+1,y+1)) ((p,(x1,y1)):t) && elem (Vazio,(x+1,y)) ((p,(x1,y1)):t) = andadireita
  | otherwise = (x,y)
   where
       ymax         = maxY ((p,(x,y)):t)
       andadireita = movimentodireita ((p,(x1,y1)):t) ((x,y+1),d)
       xmax         = maxX ((p,(x,y)):t)

{- | 
__TreparSemCaixa__

Para que o personagem seja capaz de trepar sem caixa, temos de verificar a peça imediatamente à frente do jogador (tem de ser um obstáculo) e a peça acima desta
(que tem de ser um Vazio).

Exemplo:

Considerando _(x,y)_ as coordenadas do jogador.
Caso o personagem esteja voltado para Este e se verifique que a peça imediatamente à sua frente (x+1,y) é um obstáculo (uma caixa ou um bloco) e que a peça acima 
desta (x+1,y-1) é Vazio, então o jogador assume as coordenadas (x+1,y-1).
Caso o personagem estivsse voltado para Oeste e se encontrasse dentro das mesmas condições, assumiria as coordenadas (x-1,y-1).
Independentemente da direção, caso não se verificassem as condiçõesnecessárias para a realização do movimento, o personagem não alteraria a sua posição.
-}

treparsemcaixa :: [(Peca,Coordenadas)] -> (Coordenadas,Direcao) -> Coordenadas
treparsemcaixa [] _ = (0,0)
treparsemcaixa ((p,(x1,y1)):t) ((x,y),d)
  | (elem (Caixa,(x+1,y)) ((p,(x1,y1)):t) || elem (Bloco,(x+1,y)) ((p,(x1,y1)):t)) && elem (Vazio,(x+1,y-1)) ((p,(x1,y1)):t) && d==Este = (x+1,y-1)
  | (elem (Caixa,(x-1,y)) ((p,(x1,y1)):t) || elem (Bloco,(x-1,y)) ((p,(x1,y1)):t)) && elem (Vazio,(x-1,y-1)) ((p,(x1,y1)):t) && d==Oeste = (x-1,y-1)
  | notElem (Caixa,(x+1,y)) ((p,(x1,y1)):t) || notElem (Bloco,(x+1,y)) ((p,(x1,y1)):t) || notElem (Vazio,(x+1,y-1)) ((p,(x1,y1)):t) && d==Este = (x,y)
  | notElem (Caixa,(x-1,y)) ((p,(x1,y1)):t) || notElem (Bloco,(x-1,y)) ((p,(x1,y1)):t) || notElem (Vazio,(x-1,y-1)) ((p,(x1,y1)):t) && d==Oeste = (x,y)
  | otherwise = (x,y)
 where
     xmax = maxX ((p,(x,y)):t)


{- |
__Interagir com caixa__

--| __JogadorApanhaCaixa__

Para que o jogador possa executar o movimento __InterageCaixa__ é necessário que imediatamente à sua frente exista uma caixa e que a peça acima desta seja Vazio,
sendo que quando o jogador apanha a caixa, esta é removida do mapa, sendo substituída por Vazio.
Para remover a caixa do mapa construímos a função @jogapanhacaixa@ que recorre a uma função auxiliar, a @apanhacaixa@, que recebe uma lista de peças e coordenadas
e um conjunto de peça e coordenadas, retribuindo uma nova lista.
Dado que a função @jogapanhacaixa@ recebe um mapa e as coordenadas e direção do personagem e retribui um mapa, é necessário avaliar os casos em que o jogador 
pode apanhar uma caixa.

Exemplo:

Caso o jogador esteja voltado para Este e exista uma caixa imediatamente à sua frente (condição verificada através da função @desconstroiMapa@, que comprova se 
(Caixa,(x+1,y)) pertence à lista de peças e coordenadas correspondente ao mapa), a função recorre então à auxiliar @apanhacaixa@ que utiliza essa mesma lista e 
o conjunto (Caixa,(x+1,y)) para substituir a caixa por uma nova peça, o Vazio. A função @apanhacaixa@ atua recursivamente.
De seguida, através da função @constroiMapa@, atribuindo lhe a nova lista gerada na função auxiliar, obtemos o novo mapa, isto é, sem a caixa.
-}

jogapanhacaixa :: Mapa -> (Coordenadas,Direcao) -> Mapa 
jogapanhacaixa l ((x,y),d)
            | y==0 = l
            | d==Este && x==xmax || d==Oeste && x==0 = l
            | d==Oeste && elem (Caixa,(x-1,y)) m = constroiMapa (apanhacaixa m (Caixa,(x-1,y)))
            | d==Este && elem (Caixa,(x+1,y)) m = constroiMapa (apanhacaixa m (Caixa,(x+1,y)))
            | d==Este && notElem (Caixa,(x+1,y)) m = l
            | d==Oeste && notElem (Caixa,(x-1,y)) m = l
 where
  m = desconstroiMapa (inverte l)
  xmax = maxX m

apanhacaixa :: [(Peca,Coordenadas)] -> (Peca,Coordenadas) -> [(Peca,Coordenadas)]
apanhacaixa [] _ = []
apanhacaixa ((p1,(x1,y1)):t) (p,(x,y)) | (p1,(x1,y1))==(p,(x,y)) = (Vazio,(x,y)) : t
                                       | otherwise = (p1,(x1,y1)) : apanhacaixa t (p,(x,y))

-- | __JogadorPousaCaixa__

-- | O método utilizado para o jogador pousar a caixa é idêntico ao utilizado para o jogador apanhar a caixa.

{- |
Para que o jogador possa executar o movimento __InterageCaixa__ é necessário que imediatamente à sua frente exista um Vazio, sendo que quando o jogador pousa a 
caixa, esta é inserida do mapa.
Para acrescentar a caixa do mapa construímos a função @jogapousacaixa@ que recorre a uma função auxiliar, a @pousacaixa@, que recebe uma lista de peças e coordenadas
e um conjunto de peça e coordenadas, retribuindo uma nova lista.
Dado que a função @jogapousacaixa@ recebe um mapa e as coordenadas e direção do personagem e retribui um mapa, é necessário avaliar os casos em que o jogador 
pode pousar uma caixa.

Exemplo:

Caso o jogador esteja voltado para Este e exista um Vazio imediatamente à sua frente (condição verificada através da função @desconstroiMapa@, que comprova se 
(Vazio,(x+1,y)) pertence à lista de peças e coordenadas correspondente ao mapa), a função recorre então à auxiliar @pousacaixa@ que utiliza essa mesma lista e 
o conjunto (Vazio,(x+1,y)) para substituir o Vazio por uma nova peça, uma Caixa. A função @pousacaixa@ atua recursivamente.
De seguida, através da função @constroiMapa@, atribuindo lhe a nova lista gerada na função auxiliar, obtemos o novo mapa, isto é, com a caixa.
-}

jogpousacaixa :: Mapa -> (Coordenadas,Direcao) -> Mapa 
jogpousacaixa l ((x,y),d)
     | y==0 = l
     | d==Este && x==xmax || d==Oeste && x==0 = l
     | d==Oeste && elem (Vazio,(x-1,y)) m = constroiMapa (pousacaixa m (Vazio,(x-1,y)))
     | d==Este && elem (Vazio,(x+1,y)) m = constroiMapa (pousacaixa m (Vazio,(x+1,y)))
     | d==Este && elem (Vazio,(x+1,y-1)) m && notElem (Vazio,(x+1,y)) m = constroiMapa (pousacaixa m (Vazio,(x+1,y-1)))
     | d==Oeste && elem (Vazio,(x-1,y-1)) m && notElem (Vazio,(x-1,y)) m = constroiMapa (pousacaixa m (Vazio,(x-1,y-1)))
     | d==Este && (elem (Caixa,(x+1,y)) m || elem (Bloco,(x+1,y)) m) = l
     | d==Oeste && (elem (Caixa,(x-1,y)) m || elem (Bloco,(x-1,y)) m) = l
   where
      m = desconstroiMapa (inverte l)
      xmax = maxX m

pousacaixa :: [(Peca,Coordenadas)] -> (Peca,Coordenadas) -> [(Peca,Coordenadas)]
pousacaixa ((p1,(x1,y1)):t) (p,(x,y)) | (p1,(x1,y1))==(p,(x,y)) = (Caixa,(x,y)) : t
                                       | otherwise = (p1,(x1,y1)) : pousacaixa t (p,(x,y))

-- | A função @inverte@ é usada para inverter o mapa uma vez que a função @desconstroiMapa@ aplicada nesta tarefa retribui os resultados invertidos.
inverte :: [[Peca]] -> [[Peca]]
inverte [] = []
inverte (h:t) = inverte t ++ [h]

{- |
__AndarDireita com caixa__


A função @podeavancardireita@ verifica, a partir de uma lista de peças e coordenadas e das coordenadas do jogador, se este pode ou não executar o movimento 
_AndarDireita_ enquanto carrega uma caixa.

Considerando _(x,y)_ as coordenadas do jogador:

Este movimento só é possível caso no espaço imediatamente à direita do jogador (x+1,y) e acima desse espaço (x+1,y-1), existam peças Vazio, podendo o jogador, 
neste movimento, apenas deslocar-se uma unidade no eixo das abcissas para a direita (caso se verifique a existência de um bloco ou caixa no espaço à sua direita 
e abaixo uma unidade, ou seja, em (x+1,y+1) e, caso exista um Vazio no espaço acima desse, ou seja, (x+1,y-1)) - 3ª guarda da função - ou sofrer uma situação de 
queda (caso se verifiquem os Vazios em (x+1,y) e em (x+1,y-1) e não se verifique a existência de um bloco ou caixa no espaço à sua direita e abaixo uma unidade, 
ou seja, em (x+1,y+1)) - 5ª guarda da função (a 2ª guarda representa um caso de paragem para a situação de queda).
Para a situação de queda recorremos à recursividade da função @podeavancardireita@.
Caso não se verifiquem estas condições, o jogador não altera a sua posição.
-}

podeavancardireita :: [(Peca,Coordenadas)] -> (Coordenadas,Direcao) -> Coordenadas 
podeavancardireita [] _ = (0,0)
podeavancardireita ((p,(x1,y1)):t) ((x,y),d)
   | x==xmax = (x,y)
   | y==ymax = (x+1,y-1)
   | elem (Vazio,(x+1,y)) ((p,(x1,y1)):t) && elem (Vazio,(x+1,y-1)) ((p,(x1,y1)):t) && (elem (Caixa,(x+1,y+1)) ((p,(x1,y1)):t) || elem (Bloco,(x+1,y+1)) ((p,(x1,y1)):t)) = (x+1,y)
   | (notElem (Vazio,(x+1,y)) ((p,(x1,y1)):t) || elem (Vazio,(x+1,y-1)) ((p,(x1,y1)):t)) && (elem (Caixa,(x+1,y+1)) ((p,(x1,y1)):t) || elem (Bloco,(x+1,y+1)) ((p,(x1,y1)):t)) = (x,y)
   | notElem (Caixa,(x+1,y+1)) ((p,(x1,y1)):t) || notElem (Bloco,(x+1,y+1)) ((p,(x1,y1)):t) && elem (Vazio,(x+1,y)) ((p,(x1,y1)):t) || elem (Vazio,(x+1,y-1)) ((p,(x1,y1)):t) = andadireita
   | otherwise = (x,y)
  where
     xmax = maxX ((p,(x,y)):t)
     ymax         = maxY ((p,(x,y)):t)
     andadireita  = podeavancardireita ((p,(x1,y1)):t) ((x,y+1),d)
     
{- |
__AndarEsquerda com caixa__


A função @podeavancaresquerda@ verifica, a partir de uma lista de peças e coordenadas e das coordenadas do jogador, se este pode ou não executar o movimento 
_AndarEsquerda_ enquanto carrega uma caixa.

Considerando _(x,y)_ as coordenadas do jogador:

Este movimento só é possível caso no espaço imediatamente à esquerda do jogador (x-1,y) e acima desse espaço (x-1,y-1), existam peças Vazio, podendo o jogador, 
neste movimento, apenas deslocar-se uma unidade no eixo das abcissas para a esquerda (caso se verifique a existência de um bloco ou caixa no espaço à sua esquerda 
e abaixo uma unidade, ou seja, em (x-1,y+1) e, caso exista um Vazio no espaço acima desse, ou seja, (x-1,y-1)) - 3ª guarda da função - ou sofrer uma situação de 
queda (caso se verifiquem os Vazios em (x-1,y) e em (x-1,y-1) e não se verifique a existência de um bloco ou caixa no espaço à sua direita e abaixo uma unidade, 
ou seja, em (x-1,y+1)) - 5ª guarda da função (a 2ª guarda representa um caso de paragem para a situação de queda).
Para a situação de queda recorremos à recursividade da função @podeavancaresquerda@.
Caso não se verifiquem estas condições, o jogador não altera a sua posição.
-}

podeavancaresquerda :: [(Peca,Coordenadas)] -> (Coordenadas,Direcao) -> Coordenadas
podeavancaresquerda [] _ = (0,0)
podeavancaresquerda ((p,(x1,y1)):t) ((x,y),d)
  | x==0 = (x,y)
  | y==ymax = (x-1,y-1)
  | elem (Vazio,(x-1,y)) ((p,(x1,y1)):t) && elem (Vazio,(x-1,y-1)) ((p,(x1,y1)):t) && (elem (Caixa,(x-1,y+1)) ((p,(x1,y1)):t) || elem (Bloco,(x-1,y+1)) ((p,(x1,y1)):t)) = (x-1,y)
  | (notElem (Vazio,(x-1,y)) ((p,(x1,y1)):t) || elem (Vazio,(x-1,y-1)) ((p,(x1,y1)):t)) && (elem (Caixa,(x-1,y+1)) ((p,(x1,y1)):t) || elem (Bloco,(x-1,y+1)) ((p,(x1,y1)):t)) = (x,y)
  | notElem (Caixa,(x-1,y+1)) ((p,(x1,y1)):t) || notElem (Bloco,(x-1,y+1)) ((p,(x1,y1)):t) && elem (Vazio,(x-1,y)) ((p,(x1,y1)):t) || elem (Vazio,(x+1,y-1)) ((p,(x1,y1)):t) = andaesquerda
  | otherwise = (x,y)
   where
       ymax         = maxY ((p,(x,y)):t)
       andaesquerda = podeavancaresquerda ((p,(x1,y1)):t) ((x,y+1),d)

-- | __TreparComCaixa__
{- |
Para que o personagem seja capaz de trepar com caixa, temos de verificar a peça imediatamente à frente do jogador (tem de ser um obstáculo) e as duas peças
 acima desta (que têm de ser um Vazio).

Exemplo:

Considerando _(x,y)_ as coordenadas do jogador.
Caso o personagem esteja voltado para Este e se verifique que a peça imediatamente à sua frente (x+1,y) é um obstáculo (uma caixa ou um bloco) e que as duas peças
acima desta (em (x+1,y-1) e (x+1,y-1)) são Vazios, então o jogador assume as coordenadas (x+1,y-1).
Caso o personagem estivesse voltado para Oeste e se encontrasse dentro das mesmas condições, assumiria as coordenadas (x-1,y-1).
Independentemente da direção, caso não se verificassem as condições neessárias para realizar o movimento, o personagem não alteraria a sua posição.
-}

treparcomcaixa :: [(Peca,Coordenadas)] -> (Coordenadas,Direcao) -> Coordenadas
treparcomcaixa [] _ = (0,0)
treparcomcaixa ((p,(x1,y1)):t) ((x,y),d)
  | (elem (Caixa,(x+1,y)) ((p,(x1,y1)):t) || elem (Bloco,(x+1,y)) ((p,(x1,y1)):t)) && elem (Vazio,(x+1,y-1)) ((p,(x1,y1)):t) && elem (Vazio,(x+1,y-2)) ((p,(x1,y1)):t) && d==Este = (x+1,y-1)
  | (elem (Caixa,(x-1,y)) ((p,(x1,y1)):t) || elem (Bloco,(x-1,y)) ((p,(x1,y1)):t)) && elem (Vazio,(x-1,y-1)) ((p,(x1,y1)):t) && elem (Vazio,(x-1,y-2)) ((p,(x1,y1)):t) && d==Oeste = (x-1,y-1)
  | notElem (Caixa,(x+1,y)) ((p,(x1,y1)):t) || notElem (Bloco,(x+1,y)) ((p,(x1,y1)):t) || notElem (Vazio,(x+1,y-1)) ((p,(x1,y1)):t) || notElem (Vazio,(x+1,y-2)) ((p,(x1,y1)):t) && d==Este = (x,y)
  | notElem (Caixa,(x-1,y)) ((p,(x1,y1)):t) || notElem (Bloco,(x-1,y)) ((p,(x1,y1)):t) || notElem (Vazio,(x-1,y-1)) ((p,(x1,y1)):t) || notElem (Vazio,(x-1,y-2)) ((p,(x1,y1)):t)&& d==Oeste = (x,y)
  | otherwise = (x,y)
 where
     xmax = maxX ((p,(x,y)):t)
