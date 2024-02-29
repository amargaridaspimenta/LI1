module Tarefa6_2021li1g085_Spec where


testsT6 =
  test 
  ["Tarefa 6 - Teste Resolve jogo0  " ~: ResolveJogo 5  jogo0    ~=? [AndarDireita]
  ,"Tarefa 6 - Teste Resolve jogo00 " ~: ResolveJogo 5  jogo00   ~=? []
  ,"Tarefa 6 - Teste Resolve jogo1  " ~: ResolveJogo 5  jogo0    ~=? [AndarDireita,AndarDireita]
  ,"Tarefa 6 - Teste Resolve jogo2  " ~: ResolveJogo 5  jogo0    ~=? [AndarDireita,Trepar,AndarDireita]
  ,"Tarefa 6 - Teste Resolve jogo3  " ~: ResolveJogo 5  jogo0    ~=? [AndarDireita,,AndarDireita,Trepar,AndarDireita]
  ,"Tarefa 6 - Teste Resolve jogo4  " ~: ResolveJogo 1  jogo0    ~=? Nothing
  ,"Tarefa 6 - Teste Resolve jogo5  " ~: ResolveJogo 6  jogo0    ~=? [AndarDireita,AndarDireita,AndarDireita,AndarDireita,Trepar,AndarDireita]
  ,"Tarefa 6 - Teste Resolve jogo6  " ~: ResolveJogo 10 jogo0    ~=? [AndarDireita,AndarDireita,AndarDireita,AndarDireita,AndarDireita,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda]
   ]
    
--Mapa pequeno.
jogo0 :: Jogo
jogo0 = Jogo mapa0 (Jogador (0,0) Oeste False)

--Jogador nas coordenadas da porta.
jogo00 :: Jogo
jogo00 = Jogo mapa0 (Jogador (0,0) Oeste False)

jogo1 :: Jogo
jogo1 = Jogo mapa1 (Jogador (0,1) Este False)

jogo2 :: Jogo
jogo2 = Jogo mapa2 (Jogador (0,0) Este False)

jogo3 :: Jogo
jogo3 = Jogo mapa3 (Jogador (0,1) Este False)

jogo4 :: Jogo
jogo4 = Jogo mapa4 (Jogador (0,2) Oeste False)

jogo5 :: Jogo
jogo5 = Jogo mapa5 (Jogador (0,5) Este False)

jogo6 :: Jogo
jogo6 = Jogo mapa6 (Jogador (0,0) Este False)

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
mapa4 = [[Vazio,Vazio,Vazio,Vazio],
         [Vazio,Vazio,Vazio,Porta],
         [Vazio,Vazio,Vazio,Bloco],
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



