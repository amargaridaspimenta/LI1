module Tarefa3_2021li1g085_Spec where

import Test.HUnit
import Tarefa3_2021li1g085
import Fixtures

testsT3 =
  test
    [ "Tarefa 3 - Teste Imprime Jogo m1e1" ~: "      <\n      X\n      X\nP   C X\nXXXXXXX" ~=?  show m1e1
    , "Tarefa 3 - Teste Imprime Jogo m1e2" ~: "       \n      X\n      X\nP < C X\nXXXXXXX" ~=?  show m1e2
    , "Tarefa 3 - Teste Imprime Jogo m1e3" ~: "       \n      X\n      X\nP>  C X\nXXXXXXX" ~=?  show m1e3
    , "Tarefa 3 - Teste Imprime Jogo m1e4" ~: "       \n C    X\n >    X\nP   C X\nXXXXXXX" ~=?  show m1e4
    , "Tarefa 3 - Teste Imprime Jogo m1e5" ~: m5i                                           ~=?  show m1e5
    ]
