module Tarefa1_2021li1g085_Spec where

import Test.HUnit
import LI12122
import Tarefa1_2021li1g085
import Fixtures

-- Tarefa 1
testsT1 =
  test
    [ "Tarefa 1 - Teste Valida Mapa m1r" ~: validaPotencialMapa m1 ~=? True
    , "Tarefa 1 - Teste Valida Mapa vazio" ~: validaPotencialMapa [] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com 2 portas" ~: validaPotencialMapa [(Porta, (0,0)), (Porta, (1,0))] ~=?  False
      "Tarefa 1 - Teste Valida Mapa com 2 pecas na mesma posicao" ~: validaPotencialMapa m2 ~=? False
    , "Tarefa 1 - Teste Valida Mapa com caixa que flutua" ~: validaPotencialMapa m3 ~=? False
    , "Tarefa 1 - Teste Valida Mapa com chao descontinuo" ~: validaPotencialMapa m4 ~=? False
    ]
    
