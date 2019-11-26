module TestAverages (
  testAverages
) where

import Test.HUnit
import JsonParser
import Queries

-- Testes referentes ao cálculo da média das receitas em um determinado ano.
testAverageRevenues01 = TestCase (
    do
      db <- getTestTransactions
      assertEqual "Calcula a média das receitas em 2018" 510.94500000000005 (getAvgRevenues db 2018)
    )

testAverageRevenues02 = TestCase (
  do
    db <- getTestTransactions
    assertEqual "Calcula a média das receitas em 2019" 2443.4 (getAvgRevenues db 2019)
  )

-- Testes referentes ao cálculo da média das despesas em um determinado ano.
testAverageExpenses01 = TestCase (
    do
      db <- getTestTransactions
      assertEqual "Calcula a média das despesas em 2018" 1255.0 (getAvgExpenses db 2018)
    )

testAverageExpenses02 = TestCase (
  do
    db <- getTestTransactions
    assertEqual "Calcula a média das despesas em 2019" 5250.0 (getAvgExpenses db 2019)
  )

-- Testes referentes ao cálculo da média das sobras em um determinado ano.
testAverageRemains01 = TestCase (
    do
      db <- getTestTransactions
      assertEqual "Calcula a média das sobras em 2018" (-744.0550000000001) (getAvgRemains db 2018)
    )

testAverageRemains02 = TestCase (
  do
    db <- getTestTransactions
    assertEqual "Calcula a média das sobras em 2019" (-2806.5999999999995) (getAvgRemains db 2019)
  )


testAverages = 
  TestList [
    testAverageRevenues01,
    testAverageRevenues02,
    testAverageExpenses01,
    testAverageExpenses02,
    testAverageRemains01,
    testAverageRemains02
  ]