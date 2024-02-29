module Main where

import Data.Maybe ( fromJust )
import Graphics.Gloss.Interface.Pure.Game (Event (EventKey), Key (SpecialKey), SpecialKey (KeyUp, KeyDown, KeyLeft, KeyRight, KeyEnter, KeySpace), KeyState (Down, Up))
import LI12122
    ( Direcao(Oeste, Este), Jogador(..), Jogo(..), Mapa, Peca(..), Coordenadas, Movimento (Trepar, InterageCaixa, AndarDireita, AndarEsquerda) )
import Tarefa4_2021li1g085 (moveJogador, inverte)
import Tarefa2_2021li1g085 ( constroiMapa, desconstroiMapa )
import Graphics.UI.GLUT (DataType(Int), SpecialKey ())
import Text.Html (menu)
import Graphics.Gloss hiding (light)

-- | O _type_ @EstadoGloss@ contém informação sobre o estado do jogo à medida que este decorre.
type EstadoGloss = (Menu, PecasPic, PictureJogador, PictureMenu)

{- |
 O _type_ @PecasPic@ cooresponde a uma lista de peças com a sua determinada imagem e coordenadas (as coordenadas são convertidas através de uma função para float,
de modo a fazer o mapeamento entre as peças, a sua imagem e as suas coordenadas.
-}
type PecasPic = [(Peca, (Picture, (Float,Float)))]

-- | O __type__ @PictureJogador@ e o __type__ @PecasPic@ correspondem a uma lista de Pictures.
type PictureJogador = [Picture]

type PictureMenu = [Picture]

-- | Criamos __data__ @Opcoes@ e __data__ @Menu@ de modo a criar novos tipos que facilitam o controlo do menu e do jogo através dos eventos de teclado.
data Opcoes = Jogar
            | Sair

data Menu = Controlador Opcoes
          | ModoJogo Jogo
          | YouWon


lvl1 :: Mapa
lvl1 = [[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
        [Bloco, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Vazio, Bloco],
        [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
        [Porta, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Caixa, Vazio, Vazio, Bloco],
        [Bloco, Caixa, Bloco, Bloco, Vazio, Caixa, Vazio, Bloco, Bloco, Vazio, Bloco],
        [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]

-- | As funções @altura@ e @comprimento@ indicam a respetiva altura e comprimento a que o mapa se encontra.
altura :: Float
altura = 300

comprimento :: Float
comprimento = -570

-- | A função @ld@ indica a distância entre cada célula da matriz. 
ld :: Float
ld = 116

-- | A função @estadoInicial1@ indica as condições iniciais do jogo.
estadoInicial1 :: Jogo
estadoInicial1 = Jogo lvl1 (Jogador (10,2) Oeste False)


{- |
Para executar esta parte do projeto foi necessário criar novos _types_ e _data_ capazes de conter a informação necessária de modo a aplicar as facilmente
as capacidades gráficas que a biblioteca Gloss disponibiliza.

A função @estadoGlossInicial@ recebe informação sobre as peças e as suas posições bem como as suas imagens, a imagem do jogador e as imagens utilizadas
no menu, devolvendo assim informação sobre o estado inicial do jogo.
-}
estadoGlossInicial :: PecasPic -> PictureJogador -> PictureMenu -> EstadoGloss
estadoGlossInicial texturas picj picm = (Controlador Jogar, texturas, picj, picm)

-- | Através da função @getMapa@, tendo o Jogo, é possível obter o mapa do jogo.
getMapa :: Jogo -> Mapa
getMapa (Jogo mapa (Jogador (x,y) dir bool)) = mapa

-- | Através da função @getJogador@, tendo o Jogo, é possível obter o Jogador, que contém informações sobre as suas coordenadas, direção e se carrega ou não caixa.
getJogador :: Jogo -> Jogador
getJogador (Jogo m (Jogador (x,y) dir bool)) = Jogador (x,y) dir bool

fr :: Int
fr = 35

-- | A função @dm@ retribui a dimensão da janela do jogo que, neste caso, optamos por utilizar a funcionalidade "FullScreen".
dm :: Display
dm = FullScreen

{-- | 
A função @desenhaEstado@ recebe um EstadoGloss, ou seja, recebe informações sobre o jogo, neste caso sobre o menu, o mapa, o jogador e as imagens correspondentes
a cada um deles. Tendo em conta essas informções, a função reproduz assim uma picture.
De modo a obter estas informações, a função recorre a funções auxiliares como a @getMapa@ (obtém o respetivo mapa), a @getJogador@ (obtém informações sobre o 
jogador), a @desenhaMapa@ (responsável por ler o _type_ PecasPic e reproduzi-lo) e a @desenhaMenu@ (recebe um EstadoGloss e retribui uma picture).
-}

desenhaEstado :: EstadoGloss ->  Picture
desenhaEstado (ModoJogo jogo, pecas, picj,picm) = Pictures desenho
   where
        mapa        = getMapa jogo
        jogador     = getJogador jogo
        desenhoMapa = desenhaMapa comprimento altura mapa pecas
        desenhoJog  = desenhaJog jogador picj
        desenho     = desenhoJog : desenhoMapa
desenhaEstado (Controlador Jogar,pecas,picj,picm) = desenhoMenu (Controlador Jogar,pecas,picj,picm)
 where
        desenhoMenu = desenhaMenu
desenhaEstado (Controlador Sair,pecas,picj,picm) = desenhoMenu (Controlador Sair,pecas,picj,picm)
 where
        desenhoMenu = desenhaMenu
desenhaEstado (YouWon,pecas,picj,picm) = Pictures [ scale 1.8 1.8 (picm !! 0),Translate (210) (-250) $ scale 2.4 2.4 (picm !! 4)]

{- |
A função @desenhaMapa@ recebe dois floats, recebe um mapa e recebe o _type_ PecasPic que através das funções auxiliares @desenhaLinha@ e @desenhaPeca@ faz o 
mapeamento entre a peça e as suas coordenadas com a sua respetiva imagem, retribuindo assim uma lista de Pictures.
A função representada por @linha@ recorre à função auxiliar @desenhaLinha@ que desenha a _head_ do mapa. Por sua vez, a função representada por @restomapa@
recorre recursivamente à funcão @desenhaMapa@. 
Assim, criando uma lista através da concatenação das funções @linha@ e @restomapa@, é possível desenhar o resto da mapa. 

Dificuldade:
- Inicialmente senti alguma dificuldade na execução desta parte da tarefa, contudo, através de vídeos associados à utilização do Gloss, consegui perceber a sua 
lógica e aplicação. 
-}
desenhaMapa :: Float -> Float -> Mapa -> PecasPic -> [Picture]
desenhaMapa _ _ [] _= []
desenhaMapa x y (h:t) pecas = linha ++ restomapa
  where linha = desenhaLinha x y h pecas
        restomapa = desenhaMapa x (y-ld) t pecas -- (y-ld) é uma translação aplicada ao y que garante que as linhas são desenhadas abaixo umas das outras.

{- |
A função @desenhaLinha@ atua do mesmo modo que a função @desenhaMapa@, no entanto em vez de receber um mapa recebe uma lista de peças.
A função representada por @peca@ recorre à função auxiliar @desenhaPeca@ que desenha a _head_ da lista de peças. Por sua vez, a função representada por 
@restopeca@ recorre recursivamente à funcão @desenhaLinha@. 
Assim, criando uma lista através da junção de ambas as funções, é possível desenhar o resto das linhas. 
-}
desenhaLinha :: Float -> Float -> [Peca] -> PecasPic -> [Picture]
desenhaLinha _ _ [] _= []
desenhaLinha x y (h:t) pecas = peca : restopeca
  where peca = desenhaPeca x y h pecas
        restopeca = desenhaLinha (x+ld) y t pecas -- (x+ld) é uma translação aplicada ao x que garante que as peças são desenhadas lado a lado, da esquerda para direita.

{- | 
A função @desenhaPeca@ é semelhante às anteriores, sendo que a diferença reside que em vez de um mapa ou uma lista de peças, esta função recebe para além de
dois floats uma só peça.
Esta função transpõe para a posição @x@ e @y@ e imagem da peça que apresenta as coordenadas @(x,y)@.
As funções auxiliares @floatX@ e @floatY@ transformam as coordenadas (números inteiros) em floats, uma vez que as imagens podem precisar de translações individuais.
-}
desenhaPeca :: Float -> Float -> Peca -> PecasPic -> Picture
desenhaPeca x y peca pecas = Translate floatX floatY primeirapeca
  where procurapecas = (fromJust . lookup peca) pecas -- na lista dada no _type_ PecasPic, a função lookup, nessa lista apenas as peças.
        primeirapeca = fst procurapecas -- a função @primeirapeca@ retribui a primeira peça dessa lista.
        floatX   = ((+x) . fst . snd) procurapecas -- transforma as abcissas em floats.
        floatY   = ((+y) . snd . snd) procurapecas -- transforma as ordenadas em floats.

{- |
A função @desenhaJog@ recebe informação sobre o jogador e uma lista de imagens (que variam na direção e se este carrega ou não caixa), reproduzindo assim a 
Picture do jogador dadas a sua posição, direção e se carrega ou não caixa.
As funções auxiliares @jogX@ e @jogY@ transformam as coordenadas (números inteiros) em floats uma vez que as imagens podem precisar de translações individuais.

Exemplo: 
Se o jogador estiver voltado para este e não carregar caixa, então é dado _Translate_ da imagem do jogador (que se encontra na primeira posição da lista dada
pelo _type_ PictureJogador [picj !! 0]) para as posições @jogX@ e @jogY@.
-}

desenhaJog :: Jogador -> PictureJogador -> Picture
desenhaJog (Jogador (x, y) d c) picj
   | d==Este && c==False = Translate (jogX x) (jogY y) (picj !! 0)
   | d==Oeste && c==False = Translate (jogX  x) (jogY y) (picj !! 1)
   | d==Este && c==True = Translate (jogX x) ((jogY y)+58) (picj !! 2)
   | d==Oeste && c==True = Translate (jogX  x) ((jogY y)+58) (picj !! 3)
   | otherwise = picj !! 0

jogX :: Int -> Float
jogX x = comprimento + realToFrac (x-1)*ld + succ ld

jogY :: Int -> Float
jogY y = altura - realToFrac (y)*ld

{- |
A função @reageEvento@ reage a eventos do teclado, isto é, dadas informações sobre o jogo (recebe _EstadoGloss_) esta função aplica determinadas ações quando 
premida uma determinada tecla.

Exemplo: 
Quando estamos em _ModoJogo_, se à sua frente o personagem encontrar uma obstáculo e premirmos a tecla "Up", o personagem executa o movimento Trepar.
O mesmo se aplica aos restantes movimentos quando estamos em _ModoJogo_, variando apenas as teclas a premir dependendo da ação e da possibilidade de esta se
realizar.

Quando estamos no Menu, apenas utilizamos as teclas "Up" e "Down" para selecionar "Jogar" ou "Sair".

Quando ganhamos o jogo, ao premir a tecla "Enter" após aparecer "You Won!", retornamos ao Menu, podendo escolher se queremos voltar a jogar ou sair do jogo.
-}

reageEvento :: Event  -> EstadoGloss -> EstadoGloss
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo jogo,pecaspic, picj,picm) = (ModoJogo (moveJogador jogo Trepar), pecaspic,picj,picm)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo jogo,pecaspic, picj,picm)  = (ModoJogo (moveJogador jogo InterageCaixa),pecaspic,picj,picm)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo jogo,pecaspic, picj,picm)  = (ModoJogo (moveJogador jogo AndarDireita),pecaspic,picj,picm)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo jogo,pecaspic, picj,picm)  = (ModoJogo (moveJogador jogo AndarEsquerda),pecaspic,picj,picm)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Jogar,pecaspic,picj,picm) = (ModoJogo estadoInicial1, pecaspic,picj,picm)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Sair,pecaspic,picj,picm) = (Controlador Jogar, pecaspic,picj,picm)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Sair,pecaspic,picj,picm) = (Controlador Jogar, pecaspic,picj,picm)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Sair,pecaspic,picj,picm) = undefined
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Jogar,pecaspic,picj,picm) = (Controlador Sair, pecaspic,picj,picm)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Jogar,pecaspic,picj,picm) = (Controlador Sair, pecaspic,picj,picm)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (YouWon,pecaspic, picj,picm) = (Controlador Jogar,pecaspic,picj,picm)
reageEvento _ (ModoJogo estadoInicial1, texturas,picj,picm) = if ganhaJogo estadoInicial1 then (YouWon,texturas,picj,picm) else (ModoJogo estadoInicial1, texturas,picj,picm)
reageEvento  _ e = e
 
reageTempo :: Float -> EstadoGloss -> EstadoGloss
reageTempo _ e = e

-- | Esta função torna os _Vazio_ "transparentes". Aplicamo-la de modo a que o jogador não ficasse coberto por eles.

vazio :: Picture
vazio = Blank

{- |
A função @desenhaMenu@ é responsável por retribuir uma Picture quando dado um _EstadoGloss_, ou seja, quando tem informações sobre o estado do jogo.

Exemplo:
Quando nos encontrámos no controlador "Jogar", a função retribui, neste caso a primeira (background do menu), a quarta(botão "Play") e a terceira (botão "Exit")
imagens da lista de imagens definada no _type_ _PictureMenu_. 
Quando nos encontrámos no controlador "Sair" a função retribui o mesmo que no controlador "Jogar" pois encontram-se no mesmo estado.
Por sua vez, quando ganhamos o jogo, a função retribui apenas a primeira e quinta imagens.

De modo a ajustar as imagens, em alguns casos, foi necessário manipular a escala das mesmas.

Relativamente às imagens utilizadas tanto para o menu como para o próprio jogo, estas foram desenhadas e pensadas pelo grupo. 
Para dar __load__ nas imagens recorremos a um conversor de __png__ para __bmp__ online
-}
desenhaMenu :: EstadoGloss -> Picture
desenhaMenu (Controlador Jogar,estado,picj,picm) = Pictures [ scale 1.8 1.8 (picm !! 0), Translate (185) (15) $ scale 1.7 1.7 (picm !! 3) , Color white $ Translate (0) (0)  $ (picm !! 6), Color red $ Translate (0) (-190) $ (picm !! 5)]
desenhaMenu (Controlador Sair,estado,picj,picm) = Pictures [ scale 1.8 1.8 (picm !! 0), Translate (185) (15) $ scale 1.7 1.7 (picm !! 3), Color white $ Translate (0) (0) $ (picm !! 1), Color red $ Translate (0) (-190) $ (picm !! 2)]
desenhaMenu (ModoJogo (Jogo m (Jogador (x,y) dir bool)),texturas,picj,picm) = Pictures [scale 10 10 (picm !! 0)]
desenhaMenu (YouWon,estado,picj,picm) = Pictures [scale 1.8 1.8 (picm !! 0),Translate (510) (-250) $ scale 2.4 2.4 (picm !! 4)]

{- | 
A função @ganhaJogo@, através de função auxiliar @coorPorta@ retribui um bool que indica se o jogador apresenta as mesmas coordenadas que a porta.
Implementamos esta função pois é necessária para construir o caso em que se ganha o jogo, isto é, em que o jogador atinge a porta do mapa.
-}
ganhaJogo :: Jogo -> Bool
ganhaJogo (Jogo mapa (Jogador (x,y) d c)) = (x,y) == coorPorta (desconstroiMapa mapa)

coorPorta :: [(Peca,Coordenadas)] -> Coordenadas
coorPorta [] = (0,0)
coorPorta ((p,(x1,y1)):t) = if p==Porta then (x1,y1) else coorPorta t

{- |
A função @main@ recebe todas as informações do jogo, sendo ela a base do mesmo.
Esta função recebe diversas informações tais como, a dimensão do display, a cor de fundo, a função @estadoGlossInicial@, a função @desenhaEstado@,
a função @reageEvento@ e a função @reageTempo@ pelo que, através do conjunto destes dados, devolve então o nosso jogo.
Para além disso, todas as imagens mencionadas nas funções anteriores têm de ser carregadas na @main@, através da funcionalidade _loadBMP_.
-}

main :: IO ()
main = do
  bloco <- loadBMP "block.bmp"
  caixa <- loadBMP "almofada.bmp"
  porta <- loadBMP "cama.bmp"
  dudeE <- loadBMP "personagemE.bmp"
  dudeO <- loadBMP "personagemO.bmp"
  dudecaixaE <- loadBMP "personagemcaixaE.bmp"
  dudecaixaO <- loadBMP "personagemcaixaO.bmp"
  picmenu <- loadBMP "background.bmp"
  playb <- loadBMP "menu.bmp"
  playc <- loadBMP "menuplay.bmp"
  exit  <- loadBMP "menuexit.bmp"
  exit2 <- loadBMP "menuexit2.bmp"
  pillowdude <- loadBMP "pillowdude.bmp"
  youwon <- loadBMP "youwon.bmp"
  play
    dm
    white
    fr
    (estadoGlossInicial
    [
      (Bloco,(bloco,(0,0))),
      (Caixa,(caixa,(0,0))),
      (Porta,(porta,(0,0))),
      (Vazio,(vazio,(0,0)))
    ]
     [
         dudeE,
         dudeO,
         dudecaixaE,
         dudecaixaO
     ]
       [
         picmenu,
         playb,
         exit,
         pillowdude,
         youwon,
         exit2,
         playc
       ]
     )
    desenhaEstado
    reageEvento
    reageTempo
