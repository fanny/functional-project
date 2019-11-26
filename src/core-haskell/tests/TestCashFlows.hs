module TestCashFlows (
  testCashFlows
) where

import Test.HUnit
import JsonParser
import Queries

-- Testes referentes ao fluxo de caixa em um determinado mês e ano.
testCashFlow01 = TestCase (
    do
      db <- getTestTransactions
      assertEqual "Retorna o fluxod de caixa em Janeiro de 2018" [(2,41812.6)] (getCashFlow db 2018 0)
    )

testCashFlow02 = TestCase (
  do
    db <- getTestTransactions
    assertEqual "Retorna o fluxod de caixa em Maio de 2018" [(4,57822.57)] (getCashFlow db 2018 4)
  )

testCashFlow03 = TestCase (
  do
    db <- getTestTransactions
    assertEqual "Retorna o fluxod de caixa em Janeiro de 2019" [(2,130801.66),(3,135209.42)] (getCashFlow db 2019 0)
  )

testCashFlow04 = TestCase (
  do
    db <- getTestTransactions
    assertEqual "Retorna o fluxod de caixa em Abril de 2019" [(1,153797.14),(8,154276.18000000002)] (getCashFlow db 2019 3)
  )

testCashFlows = 
  TestList [
    testCashFlow01,
    testCashFlow02,
    testCashFlow03,
    testCashFlow04
  ]