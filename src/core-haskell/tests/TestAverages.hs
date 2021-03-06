module TestAverages (
  testAverages
) where

import Test.HUnit
import JsonParser
import Queries

-- Testes referentes ao cálculo da média das receitas em um determinado ano.
testAverageRevenues = TestCase (
    do
      db <- getTestTransactions
      assertEqual "Calcula a média das receitas em 2018" 510.94500000000005 (getAvgRevenues db 2018)
      assertEqual "Calcula a média das receitas em 2019" 2443.4 (getAvgRevenues db 2019)
      assertEqual "Calcula a média das receitas em 2015" 0 (getAvgRevenues db 2015)
    )

-- Testes referentes ao cálculo da média das despesas em um determinado ano.
testAverageExpenses = TestCase (
    do
      db <- getTestTransactions
      assertEqual "Calcula a média das despesas em 2018" 1255.0 (getAvgExpenses db 2018)
      assertEqual "Calcula a média das despesas em 2019" 5250.0 (getAvgExpenses db 2019)
      assertEqual "Calcula a média das despesas em 2016" 0 (getAvgExpenses db 2016)
    )

-- Testes referentes ao cálculo da média das sobras em um determinado ano.
testAverageRemains = TestCase (
    do
      db <- getTestTransactions
      assertEqual "Calcula a média das sobras em 2018" (-744.0550000000001) (getAvgRemains db 2018)
      assertEqual "Calcula a média das sobras em 2019" (-2806.5999999999995) (getAvgRemains db 2019)
      assertEqual "Calcula a média das sobras em 2016" 0 (getAvgRemains db 2016)
    )

testAverages = 
  TestList [
    testAverageRevenues,
    testAverageExpenses,
    testAverageRemains
  ]