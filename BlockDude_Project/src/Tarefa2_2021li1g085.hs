module Tarefa2_2021li1g085 where

import LI12122

{- | 
A função @constroiMapa@ é uma função que dada uma determinada lista de peças e coordenadas produz um mapa, isto é, uma lista de listas de peças, sendo estas
respetivamente compostas pelas linhas da matriz que constituem o mapa.
Enquanto que a lista de peças e coordenadas não apresenta os espaços Vazio do mapa, este exibe essas mesmas peças.

Quanto à função @desconstroiMapa@, esta realiza o inverso da função @constroiMapa@, ou seja, dado um determinado mapa, esta função é capaz de atribuir a cada peça
as suas coordenadas, ocultando da sua lista as peças correspondentes a Vazio. 
-}
constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = []
constroiMapa (h:t) = constroimapa (h:t) (constroilistaPecas (h:t))

desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa [] = []
desconstroiMapa ([]:t) = desconstroiMapa (inverter t)
desconstroiMapa l  = desconstroi (inverter l) (0,y-1)
    where
        y = length l

{- | 
Criamos uma função que, dada uma lista de peças e coordenadas, retribui apenas uma lista das coordenadas.

Exemplo:
Tendo uma lista de peças e coordenadas, como por exemplo, [(Bloco,(0,2)),(Vazio,(1,1)),(Caixa,(1,2)),(Vazio,(1,0))], através da função @retiracoor@
é possível obter uma lista constituída apenas pelas coordenadas, que seria, neste caso, [(0,2),(1,1),(1,2),(1,0)].
-}
retiracoor :: [(Peca,Coordenadas)] -> [Coordenadas]
retiracoor [] = []
retiracoor ((p,(x1,y1)):t) = (x1,y1) : retiracoor t

{- | 
Tal como comentado previamente, a lista de peças e coordenadas que é atribuída à função @constroiMapa@ não apresenta as peças Vazio, pelo que, é
assim necessário construir uma função que irá atribuir essas mesmas peças.
Deste modo, através da função @constroiVazios@, dada uma lista de coordenadas (lista 1) e uma outra lista de peças e coordenadas (lista 2), esta função 
irá verificar se as coordenadas da lista 1 pertecem às coordenadas da lista 2. Para isso, recorremos à função auxiliar @retiracoor@, que constroi uma lista
de coordenadas a partir da lista 2. 
Dado isto, caso a _head_ da lista 1 pertença à lista 2, a função @constroiVazios@ recorre à função auxiliar @atribuiPeca@ que retribui a peça que corresponde
às coordenadas da _head_ da lisa 1. Caso não se verifique que a _head_ da lista 1 pertence à lista 2, a função atribui a essa coordenada um Vazio e verifica o
resto da lista. 
-}

constroiVazios :: [Coordenadas] -> [(Peca,Coordenadas)] -> [Peca]
constroiVazios [] _ = []
constroiVazios _ [] = []
constroiVazios ((x1,y1):t) ((p,(x,y)):tl)
    | (x1,y1) `elem` (retiracoor ((p,(x,y)):tl)) = atribuiPeca (x1,y1) ((p,(x,y)):tl) : constroiVazios t ((p,(x,y)):tl)
    | otherwise = Vazio : constroiVazios t ((p,(x,y)):tl)

atribuiPeca :: Coordenadas -> [(Peca,Coordenadas)] -> Peca
atribuiPeca _ [] = Vazio 
atribuiPeca (x,y) ((p,(x1,y1)):t)
    | x1 == x && y1 == y = p
    | otherwise = atribuiPeca (x,y) t

{- | 
Dada uma lista de peças e coordenadas, a função @constroilistaPecas@ retribui uma lista de peças. Esta função recorre a várias funções auxiliares, sendo
a primeira a função @retiracoor@, que irá criar uma lista de coordenadas a partir da lista de peças e coordenadas introduzida. 
Tendo a lista de coordenadas, a função recorre à função @parcoormax@, que retribui um par de coordenadas, sendo este par as coordenadas máximas
da matriz que, de seguida, serão usadas pela função @ordenacoord@ de modo a obter uma lista de coordenadas ordenadas. 
Por fim, a função recorre à função @constroiVazios@, que dada a lista de coordenadas já ordenada e a lista de peças e coordenadas, retribui uma lista de 
peças que inclui os Vazio. 
-}

constroilistaPecas :: [(Peca,Coordenadas)] -> [Peca]
constroilistaPecas [] = []
constroilistaPecas (h:t) = constroiVazios (ordenacoord (parcoormax (retiracoor (h:t)))) (h:t)

{- |
A função @ordenacoord@ ordena as coordenadas @x@ e @y@ que, recorrendo às funções auxiliares @ordenaX@ e @ordenaY@, retribui uma lista de coordenadas ordenadas. 
-}
ordenacoord :: Coordenadas -> [Coordenadas]
ordenacoord (x,y) = reverse (ordenaY (x,y))

ordenaX :: Coordenadas -> [Coordenadas]
ordenaX (x,y) = if x<0 then [] else (x,y) : ordenaX (x-1,y)


ordenaY :: Coordenadas -> [Coordenadas]
ordenaY (x,y) = if y<0 then [] else ordenaX (x,y) ++ ordenaY (x,y-1)

{- |
A função @constroiMapaAux@ retribui um mapa quando dada uma lista de peças e uma outra lista de peças e coordenadas. 
Dado que as listas que constituem o mapa são constituídas pelas peças que compõem as linhas da matriz, estas mesmas listas terão uma dimensão igual ao 
número de colunas da matriz, ou seja, o número de elementos dessas listas é igual ao @x@ máximo, pelo que, uma das funções auxiliares que iremos utilizar,
será a função @maxX@ que devolve a abcissa máxima da matriz. 
A funçao @constroiMapaAux@ recorre às funções auxiliares @drop@ e @take@, pois através delas conseguimos controlar o tamanho e a formação das listas. 
Como a função @take@ devolve os @n@ primeiros elementos da lista, e como a função @drop@ "elimina" esses @n@ elementos da lista, através da construção 
desta função recursiva é nos possivel obter a estrutura do mapa.
-}

constroimapa :: [(Peca,Coordenadas)] -> [Peca] -> Mapa
constroimapa [] _ = []
constroimapa _ [] = []
constroimapa ((p,(x1,y1)):tl) (h:t) = take n (h:t) : constroimapa ((p,(x1,y1)):tl) (drop n (h:t))
 where
   n = maxX((p,(x1,y1)):tl)+1

-- | A função @listaX@, dada uma lista de coordenadas, retribui uma lista composta apenas pelas abcissas.

listaX :: [Coordenadas] -> [Int]
listaX [] = []
listaX  ((x,y):t) = x : listaX t

-- | A função @listaY@, dada uma lista de coordenadas, retribui uma lista composta apenas pelas ordenadas.

listaY :: [Coordenadas] -> [Int]
listaY [] = []
listaY ((x,y):t) = y : listaY t

-- | A função @maxX@ devolve a abcissa máxima da matriz recorrendo às funções auxiliares @listaX@ e @retiracoor@.
maxX:: [(Peca,Coordenadas)] -> Int
maxX [] = 0
maxX l = maximum (listaX (retiracoor l))

-- | A função @maxY@ devolve a ordenada máxima da matriz recorrendo às funções auxiliares @listaY@ e @retiracoor@.

maxY :: [(Peca,Coordenadas)] -> Int
maxY [] = 0
maxY l = maximum (listaY (retiracoor l))

-- | A função @parcoormax@ devolve um par de coordenadas, sendo este par as coordenadas máximas da matriz.

parcoormax :: [Coordenadas] -> Coordenadas
parcoormax [] = (0,0)
parcoormax ((x,y):t) = (maximum (listaX ((x,y):t)) , maximum (listaY ((x,y):t)))


{- | 
A função @removeVazio@ é responsável por remover os Vazio de uma lista de peças e coordenadas e devolver, de seguida, essa nova lista. 
Para isso a função verifica se a peça da _head_ da lista corresponde a Vazio e, se isto se verificar, a função retira esse conjunto de peças e coordenadas
da lista e atua recursivamente sobre a função. Caso a peça da _head_ da lista não corresponda ao Vazio, então essa peça não é removida e a função atua 
de forma recursiva até não haverem mais elementos na lista. 
-}
removeVazio :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
removeVazio [] = []
removeVazio ((p,(x1,y1)):t) = if p==Vazio then removeVazio t else (p,(x1,y1)) : removeVazio t


desconstroi :: Mapa -> (Int,Int) -> [(Peca,Coordenadas)]
desconstroi [] _ = []
desconstroi ([]:t) (n1,n2)  = removeVazio (desconstroi t (0,n2-1))
desconstroi ((x:xs):t) (n1,n2) = removeVazio ((x,(n1,n2)) : desconstroi (xs:t) (n1+1,n2))

{- |
Dado que a função @desconstroiMapa@ retribuía a lista de peças e coordenadas ao contrário, usamos a função @inverter@ que, dado um mapa com uma determinada
sequência, retribui a sequência inversa, compensando assim a função @desconstroiMapa@. 
-}
inverter :: Mapa -> Mapa
inverter [] = []
inverter (h:[]) = [h]
inverter (h:t) = inverter t ++ [h]
