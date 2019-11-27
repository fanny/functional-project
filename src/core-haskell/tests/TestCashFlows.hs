module TestCashFlows (
  testCashFlows
) where

import Test.HUnit
import JsonParser
import Queries

-- Testes referentes ao fluxo de caixa em um determinado mês e ano.
testCashFlow = TestCase (
    do
      db <- getTestTransactions
      assertEqual "Retorna o fluxo de caixa em Janeiro de 2018" [(1,43980.15),(2,41812.6)] (getCashFlow db 2018 0)
      assertEqual "Retorna o fluxo de caixa em Maio de 2018" [(1,57143.13),(4,57822.57)] (getCashFlow db 2018 4)
      assertEqual "Retorna o fluxo de caixa em Janeiro de 2019" [(1,131301.66),(2,130801.66),(3,135209.42)] (getCashFlow db 2019 0)
      assertEqual "Retorna o fluxo de caixa em Abril de 2019" [(1,153797.14),(8,154276.18000000002)] (getCashFlow db 2019 3)
    )

testCashFlows = 
  TestList [
    testCashFlow
  ]